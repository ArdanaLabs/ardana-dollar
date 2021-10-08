# UTXO design

Where the endpoint-spec describes the Schema design, this document will describe the Validators, and Monetary Policy scripts that those endpoints will call.

This will cover all on-chain components necessary to implement the endpoint-spec.

-- TODO revise endpoint spec to match script behaviors and list script actions that arise from each endpoint call.
-- TODO resolve AdminState/VaultState bottleneck? (price token and vaultParams)
-- TODO how to track un-backed $dUSD?
-- TODO negative number conditions
-- TODO liquidation seizure in advance
-- TODO debt and surplus auction
-- TODO $dUSD Savings system
## Specification Conventions

In order to simplify and clarify the specification, this document makes a few adopts a few basic conventions
This document describes the ardana-dollar project as a lower-level description of the EUTXO transaction behaviors required by the protocol.

Specifically this will describe all Monetary Policies, Validators, and the relative Native Tokens required for the protocol to function as described in the endpoint-spec, and how they relate to each other.

Specifications for Monetary Policies will describe all native tokens controlled by that policy (by token name), as well as datum types associated with each token (ie. each token is presumed to be in a UTXO with a value of exactly 1 token,  the datum of this UTXO will always be of the same type as the Token name). Minting and burning policies for each token (if they have separate logic), and the expected inputs and outputs of those transactions.

Specifications for Validators will describe the Datum / redeemer, validation rules (including Redeemer pattern matches) and the expected inputs and outputs of these transactions.

Validator Datum and Redeemer types are documented using a state/action scheme, the datum will always reflect a State Token held by the validator script, which should be verified to match before trusting any explicit Datum argument. Action types are presented within the spec as Sum constructors with product fields for convenience, within the codebases the Sum constructors should carry product types as parameters, such that no safety issues occur during field access.

Types may be listed as primitives (though semantic newtypes should be used where possible), Or they may include types from Haskell's `base`, the Plutus Api, the CommonTypes section, or types otherwise defined on other components.

Native tokens may be of several types
- Distributed tokens - tokens given to users may be fungible or non-fungible - these will always have a `$` prefix
- State tokens - used to manage and identify datum, either for a Validator script to use for scriptwide-state, or for a single user's state, in both cases, these are non-fungible
  State tokens 'for a user' generally indicates that the `address` or similar parameter must match that of the user.
- Permission Tokens - these tokens, by nature of being in a transaction, signify permission to perform a given type of action in some other component, this is used for extensible permission systems where validation of one action can be delegated to other scripts ad-hoc.
- Signed Tokens - These Tokens carry `SignedMessage tokenDatumType` rather than `tokenDatumType` and are signed by a designated wallet to signify off-chain authority on a particular topic. Typically used for data that would otherwise bottleneck the system.

Many State and Permission tokens function as witnesses to prove some prior event or state, or to show the provenance of other transaction data.

note: though `dUSD` is used below,  this is in fact a parameter-dependant token name based on `"d" <> Parameters.peggedCurrency`, by adjusting this and corresponding parameters throughout the protocol, we can easily create dGBP etc.
note: The different Address types each indicate how transactions should be authenticated
Address - This indicates a Script or a Wallet, we should verify that the transaction includes a utxo from this address OR is signed by the address
PubKeyHash - This indicates a wallet, we should verify that the transaction is signed by this PubKeyHash if necessary
ValidatorHash - This indicates a script, we should check for an included utxo from this address.

## Constants

-- TODO
stabilityFeeBaseRate
vaultStateExpiryTime

## Common Types
```haskell
VaultConfig
  { assetClass :: AssetClass
  , active :: Bool
  , stabilityFeeMultiplier :: Rational
  }
```

Purpose: the `VaultConfig` type is used to describe settings for one or all vaults using the `VaultConfig.assetClass`.

## One-shot Tokens

These are non-fungible tokens that should be parameterized only on  UTXO inputs, as is shown in the `Currency` use case example in the Plutus MonoRepo.

One-shot tokens within the system are:

### GovernanceState token

  Token Type: State token for script

  purpose: allows us to manage stateful data relating to totals of important funds held by the Governance Validator

  carries datum:
  ```haskell
  GovernanceState { stakeTotal :: Integer
                  , rewardsTotal :: Value
                  , lastEpoch :: PosixTime
                  }
  ```

  initialized to
  ```haskell
  GovernanceState { stakeTotal = 0
                  , rewardsTotal = mempty
                  , lastEpoch = currentTime
                  }
  ```

  mint: can always mint.

  burn: can never be burned

  inputs:
- fee/collateral UTXO

  outputs:
- fee/collateral UTXO remainder -> user Wallet
- GovernanceState (MINTED) -> Governance Validator Script

### AdminState token

  Token type: State token for script, this is also a Signed token, which allows the data to be validated and used in many transactions simultaneously without a bottleneck. the token and corresponding datum are used as a source of truth off-chain.

  purpose: allows us to manage stateful data relating to Admin-level configuration, for the purpose of propigating state from Admin ownerAddress to all vaults.

  carries datum `SignedMessage AdminState` where:
  ```haskell
  AdminState
    { configs :: [VaultConfig],
    , liquidationBenefitCap :: Rational
    , liquidationBenefitFloor :: Rational
    , minCollateralRatio :: Rational
    , lastUpdated :: PosixTime
    , savingsAddress :: Maybe Address
    }
  ```
  initialized to
  ```haskell
  AdminState
    { configs = [],
    , liquidationBenefitCap = 1 % 5 -- 20%
    , liquidationBenefitFloor = 1 % 20 -- 5%
    , minCollateralRatio = 3 % 1 -- 150%
    , lastUpdated = currentTime
    , savingsAddress = Nothing -- at stablecoin launch, there will be no savings address
    }
  ```

  *known issue: the `configs` can hypothetically expand until it is unusable, this should be replaced with a record token*
  *known issue: we may need to extend this AdminState type to manage the percentage of funds that go to the Savings address*
  *known issue: this essentially addes a keeper to the system, ideally we will eliminate this*

## Governance Minting Policy
parameters:
```haskell
GovStateMintingParams { governanceScriptAddress :: ValidatorHash
                      , governanceStateCurrencySymbol :: CurrencySymbol
                      , ownerAddress :: PubKeyHash
                      }
```

Purpose: The Governance Minting Policy mints various tokens used primarily by the Governance Validator.

the `governanceScriptAddress` and `governanceStateCurrencySymbol` parameters are used to securely identify the correct state tokens and script instance to prevent supplying structures in their place.

### UserStakeDetail token

  Token Type: State token for user

  purpose: allows us to manage stateful data relevant to a User's interaction with the Governance Validator, which is a stake pool structure (users deposit $DANA Tokens (minted externally) into the pool so that their votes can be counted and rewards can be accurately distributed).

  carries datum:
  ```haskell
  UserStakeDetail { userAddress :: Address
                  , amountStaked :: Integer
                  , lastReward :: PosixTime
                  , lockedUntil :: Maybe PosixTime
                  , lockedBy :: Maybe Address
                  }
  ```

  initialized to
  ```haskell
  UserStakeDetail { userAddress = DepositAct.address -- determined by validator
                  , amountStaked = DepositAct.amount -- determined by validator
                  , lastReward = currentTime
                  , lockedUntil = Nothing
                  , lockedBy = Nothing
                  }
  ```

  mint & burn: must include the GovernanceState UTXO

  *known issue: we need to ultimately allow vote cancellation*

  *known issue: the reward distribution system is subject to change as we find an optimal implementation*

  inputs:
- fee/collateral UTXO
- GovernanceState UTXO (from Governance Validator)

  outputs:
- fee/collateral UTXO remainder -> user Wallet
- GovernanceState UTXO -> Governance Validator
- UserStakeDetail (MINTED) -> Governance Validator


### LastEpochScriptState token (1 per epoch)

  Token Type: State token for script, only needed on specific actions.

  purpose: This datum is needed as part of the reward distribution procedure, to record the exact state of the rewards and stakepool sizes so that they might be used to calculate reward dividends for each staker of $DANA at the end of a given reward period/epoch.

  carries datum:
  ```haskell
  LastEpochScriptState { stakeTotal :: Integer
                       , rewardsTotal :: Value
                       }
  ```

  initialized to
  ```haskell
  LastEpochScriptState { stakeTotal = GovernanceState.stakeTotal
                       , rewardsTotal = GovernanceState.rewardsTotal
                       -- values determined by Validator.
                       }
  ```

  mint & burn: must include the GovernanceState UTXO

  *known issue: this token/UTXO/datum may need to be duplicated in order to parallelize the reward distribution procedure*

  *known issue: at time of writing (Aug 28/2021) we are investigating a proof-of-concept that will change reward distribution design significantly. components relating to Reward distribution are expected to be updated and are not a stable portion of the spec*

  inputs:
- fee/collateral UTXO
- GovernanceState UTXO (from Governance Validator)

  outputs:
- fee/collateral UTXO remainder -> user Wallet
- GovernanceState UTXO -> Governance Validator
- LastEpochScriptState (MINTED) -> Governance Validator


### CanLockStake Token

  Token Type: Permission Token for a Script to have additional rights to modify Governance states.

  Purpose: This token is used by a ProposalFactory during voting and/or vote cancellation in order to prevent infinite voting exploits.  In particular, can force a user to lock their staked $DANA tokens until the proposal voting period ends.

  carries no datum

  mint & burn:
- must include the GovernanceState UTXO
- must include signature from `ownerAddress`

  inputs:
- fee/collateral UTXO
- GovernanceState UTXO (from Governance Validator)

  outputs:
- fee/collateral UTXO remainder -> user Wallet
- GovernanceState UTXO -> Governance Validator
- CanLockStake (MINTED) -> ProposalFactory Validator


## Governance Validator
parameters:
```haskell
GovernanceValidatorParams { governanceStateCurrencySymbol :: CurrencySymbol
                          , ownerAddress :: PubKeyHash
                          , danaTokenAssetClass :: AssetClass
                          }
```

Datum:
```haskell
GovernanceState
```
defined as a State token datum under _One Shot Tokens_

Redeemer:
```haskell
= DepositAct { amount :: Integer
             , address :: Address
             }
| WithdrawAct { amount :: Integer
              , address :: Address
              }
| ProvideRewardAct { value :: PlutusTx.Value }
| ClaimRewardAct { address :: Address
                 , epoch :: Integer
                 }
| FinishRewardsForEpochAct
| RegisterProposalFactoryAct { scriptAddress :: ValidatorHash }
| LockForVotingAct { voterAddress :: Address
                   , lockedUntil :: Integer}
| TriggerEpochAct
```

Purpose:
The Governance Validator controls transactions relating to two main protocol features
- measuring Vote weight of $DANA holders
- distributing dividends and rewards to $DANA holders

The Governance Validator is meant to integrate with any number of `ProposalFactory` validators and their corresponding `IndividualProposal`s

scope notes:
- since the Governance Minting Policy is Parameterized on the address of the Governance Validator, the governance validator can compute the `govMintingPolicyCurrencySymbol`.
- whenever referring to UserStakeDetail, it must be verified that the contract always contains exactly 1 user stake detail token per user address.

### DepositAct

Purpose: The user adds $DANA to their staked totals, may mint UserStakeDetail.

the `DepositAct.address` field is purposefully available to be a wallet or script address as a user may want to delegate control over stake and over rewards to an external contract.

Validation rules:
- user must have provided 1 or more UTXOs containing a total of at least `DepositAct.amount` of $DANA (Deposit UTXOs)
- user may have included a `UserStakeDetail` from the Governance validator script matching their `DepositAct.address`.

  if not, then one should be Minted from `GovernanceMintingPolicy`, with the user's `address` as the `address` parameter, currentTime as `lastReward` parameter, and `Nothing` as the `lockedUntil` and `lockedBy` parameters,

  in both cases, the resulting UserStakeDetail should be sent (back) to the Governance Validator script address.

In order to minimize transaction sizes, the $DANA stake pool UTXOs should consolidate with previous deposits from all users, rather than directly transfer).
- any Wallet can deposit $DANA for Any address onchain. there is no restriction.

inputs:
- fee/collateral UTXO (from USER)
- deposit UTXO (1 or more) (from USER)
- (Optional) - consolidated stake deposit UTXO (from Governance Validator Script)
- GovernanceState token  (from Governance Validator Script)
- (optional UserStakeDetail Token) (from Governance Validator Script)

outputs:
- fee/collateral UTXO remainder -> user Wallet
- deposit UTXO -> Governance Validator script (consolidated)
- GovernanceState token -> Governance Validator script (increment `GovernanceState.stakeTotal` by `DepositAct.amount`)
- UserStakeDetail Token (MINTED?) -> Governance Validator script (increment `UserStakeDetail.amountStaked` by `DepositAct.amount` OR mint fresh with `DepositAct.amount` as the initial value (other values should be initialized to defaults, `UserStakeDetail.address` should match `DepositAct.address`).

### WithdrawAct

Purpose: The user removes $DANA from their staked totals , may burn UserStakeDetail.

Validation Rules:
- if UserStakeDetail for the user's address is not included in the tx, fail.
- if amount specified in WithdrawAct.amount is greater than the balance of the `UserStakeDetail.amount`, fail
- transaction must be signed by `UserStakeDetail.address` if it is a PubKeyHash, or include a utxo from the validator if it is a ValidatorHash.
- if `UserStakeDetail.lockedUntil` is a future PosixTime, fail.
- if the UserStakeDetail token is being burned, then the user `WithdrawAct.amount` must equal `UserStakeDetail.amountStaked` and vice versa, the token must be burned they are equal. This is because the user's staked amount will be zero and we shouldn't keep unneccessary data/utxos around.

*known issue: locking mechanisms expected to change*

inputs:
- fee/collateral UTXO (from USER)
- GovernanceState token  (from Governance Validator Script)
- UserStakeDetail token  (from Governance Validator Script)
- Staked $DANA UTXO (from Governance Validator Script)

outputs:
- fee/collateral UTXO remainder -> user Wallet
- GovernanceState token -> Governance Validator script (decrement `GovernanceState.stakeTotal` amount by `WithdrawAct.amount`)
- UserStakeDetail Token -> Governance Validator script (decrement `UserStakeDetail.amountStaked` by `WithdrawAct.amount`)
- Withdrawal $DANA UTXO (contains WithdrawAct.amount of Dana) -> User Wallet
- (Optional) Remainder UTXO (contains any $DANA from input which exceeds `WithdrawAct.amount` -> Governance Validator Script

### ProvideRewardAct

Purpose: receive rewards distributed by scripts or individuals for $DANA stakers, queue these rewards for fair distribution at the end of the next reward period/epoch.

Validation Rules:
- user may include one or more UTXOs with rewards in collections of arbitrary native tokens, (reward UTXOs) these should total to match ProvideRewardsAct.value

inputs:
- fee/collateral UTXO (from USER)
- Reward UTXOs (from USER - contains arbitrary tokens)
- GovernanceState token (from Governance Validator Script)
- optional previous reward UTXO for this epoch (from Governance Validator script)

outputs:
- fee/collateral UTXO remainder -> user Wallet
- GovernanceState token -> Governance Validator script (increment `GovernanceState.rewardsTotal` by `ProvideRewardsAct.value`)
- reward UTXOs -> Governance Validator Script (consolidate to single utxo per epoch)

### ClaimRewardAct / DistributeRewardAct

Purpose: distributes rewards or assigns user amounts for individual claim later

*Known Issue: this is expected to change*

currently exploring a paginated effort where we distribute rewards directly

### FinishRewardsForEpochAct

Purpose: consolidates and queues for distribution any leftover past rewards or any tokens sent to the Governance Validator Address in error. Ends the reward distribution procedure.

Validation rules:
- `ownerAddress` must sign this transaction

*Known issue: reward distribution is expected to change, however for this action we need some witness to prove that reward distribution for this period/epoch has been completed.*

inputs:
- fee/collateral UTXO (from USER)
- GovernanceState (from Governance Validator Script)
- LastEpochScriptState (from Governance Validator Script)
- reward UTXO (for past epoch) (from Governance Validator Script)
- (optional) - other UTXOs that need to be consolidated
- (optional) - reward UTXO (for current epoch)

outputs:
- fee/collateral UTXO remainder -> user Wallet
- GovernanceState token -> Governance Validator script (set `GovernanceState.rewardsTotal` to the sum of the rewards and other utxos included for consolidation)
- LastEpochScriptState -> BURN
- reward UTXO -> Governance Validator Script (consolidated from reward UTXOs above)

### RegisterProposalFactoryAct

Purpose: to permit a `ProposalFactory` to interact with the governance validator script with enhanced capability.

Validation Rules:
- `ownerAddress` address must sign this transaction

inputs:
- fee/collateral UTXO (from USER)

outputs:
- fee/collateral UTXO remainder -> user Wallet
- CanLockStake token (MINTED) -> RegisterProposalFactoryAct.scriptAddress

### LockForVotingAct

Purpose: In Tandem with an `IndividualProposal` vote action, this is used to lock a user's staked $DANA until a voting period expires.

*Known issue: we need to enable vote cancellation to permit reasonable market actions on the part of $DANA token holders, such as purchasing additional tokens and voting a with additional resources. This action is subject to change, additionally we need to prevent malicious ProposalFactories from creating an infinite lock*

Validation rules:
- LockForVotingAct.voterAddress must have signed this transaction or include a utxo from that address if it is a validatorHash
- `LockForVotingAct.address` and `UserStakeDetail.address` must match.

inputs:
- fee/collateral UTXO (from USER)
- CanLockStake token (from ProposalFactory)
- UserStakeDetail (for LockForVotingAct.voterAddress, from Governance Validator Script)

outputs:
- fee/collateral UTXO remainder -> user Wallet
- CanLockStake token (Return to ProposalFactory)
- UserStakeDetail -> Governance Validator Script (set lockedUntil = LockForVotingAct.lockedUntil, set lockedBy = ProposalFactory address)

### TriggerEpochAct

Purpose: the first step in distributing rewards, records the rewards totals for a given reward period/epoch and resets state to track for the new epoch

note: this will rely on a constant `epochLength :: PosixTimeRange`

Validation rules:
- `GovernanceState.lastEpoch` must be at least `epochLength` less than the `currentTime`.

inputs:
fee/collateral UTXO remainder (from USER)
GovernanceState token (from Governance Validator script)

outputs:
fee/collateral UTXO remainder -> user Wallet
GovernanceState token -> Governance Validator script (set `GovernanceState.rewardsTotal` to `Value.empty`
LastEpochScriptState state token (MINTED)-> Governance Validator Script (set `LastEpochScriptState.stakeTotal` to `GovernanceState.stakeTotal`, and `LastEpochScriptState.rewardsTotal` to `GovernanceState.rewardsTotal`)

## Proposal Minting Policy (example)
parameters:
```haskell
ProposalMintingParams { proposalFactoryAddress :: Address
                      , governanceMintingCurrencySymbol :: CurrencySymbol
                      }
```

Purpose: The Proposal Minting Policy manages state tokens relating to a hypothetical Proposal Factory and/or IndividualProposal script.

note: this will not actually be implemented, it's intended as a guide for those who might need to integrate with the Governance Validator.

### ProposalState Token
  Token Type: State token for IndividualProposal Validator script.

  Carries Datum:
  ```haskell
  ProposalState
    { totalVote :: Integer
    , voterList :: [Address]
    }
  ```
  initialized to:
```haskell
  ProposalState
    { totalVote = 0
    , voterList = []
    }
  ```
  *known issue: this is one of several vote tabulation arrangements, this will change as we find the optimum implementation for vote cancellation*

  *known issue: the `voterList` can hypothetically expand until it is unusable, this should be replaced with a record token*

  Minting & Burning:
- must include an arbitrary utxo from the `proposalFactoryAddress`.

inputs:
- fee/collateral UTXO remainder (from USER)
- CanLockStakeToken (from ProposalFactory Validator)

outputs:
- fee/collateral UTXO remainder -> user Wallet
- CanLockStakeToken -> ProposalFactory Validator Address

## ProposalFactory (Example)
Parameters:
```haskell
ProposalFactoryParams { govAddress :: Address
                      . governanceMintingCurrencySymbol :: CurrencySymbol
                      }
```

Purpose:
A ProposalFactory will integrate with the Governance script directly, providing the structure and logic to make a governance decision on a particular kind of question.
Since this is an example, certain details that would otherwise be described on other parts of the spec are not available at this time.

This is an example for any service (Ardana Dex, Ardana-Dollar v2) that may perform an integration with the Governance Validator script

A Proposal Factory should carry specific logic pretaining to success conditions, quorum, minimum proposal requirements, the data which needs approval, and the voting period time limits for a proposal type, as well as any effects necessary to implement the change described (such as updating a configuration value for another script).

Datum:
`()`

Redeemer:
```haskell
CreateProposalAct
```

### CreateProposalAct
Purpose:
this would mint a new ProposalState token and send it to the precomputed address for the IndividualProposal Validator Script

Validation rules:
these may vary based on proposal logic,  an example might be that the user's $DANA UTXOs must total more than a constant Minimum Proposal Amount.

inputs:
- fee/collateral UTXO (from USER)
- $DANA token UTXO for user (from Governance Validator script)

outputs:
- fee/collateral UTXO remainder -> user Wallet
- $DANA token UTXO for user -> Governance Validator Script
- ProposalState token (MINTED)-> IndividualProposal Validator Script

## IndividualProposal (Example)
Parameters:
```haskell
data IndividualProposalParams a = IndividualProposalParams
  { proposalFactoryAddress :: Address
  , governanceMintingCurrencySymbol :: CurrencySymbol
  , proposalMintingCurrencySymbol :: CurrencySymbol
  , proposalSpecificData :: a
  }
```

Purpose:
This script manages the act of voting on a particular proposal, the precise data being voted on, and determines if the Proposal is successful.

Datum:
```haskell
ProposalState
```
defined at _Proposal Minting Policy_

Redeemer:
```haskell
= VoteAct
```

### VoteAct
Purpose: to record a user's vote on a particular proposed value.

Validation Rules
- user must not be on `ProposalState.voterList`
- user must sign transaction

inputs:
- fee/collateral UTXO (from USER)
- $DANA token UTXO for user (from Governance Validator script)
- UserStakeDetail for user (from Governance Validator script)
- CanLockStake (from ProposalFactory)
- ProposalState (from IndividualProposal)

outputs:
- fee/collateral UTXO remainder -> user Wallet
- $DANA token UTXO for user -> Governance Validator Script
- UserStakeDetail -> Governance validator Script (set `UserStakeDetail.lockedBy` to IndividualProposal Validator Address and `UserStakeDetail.lockedUntil` to the end of the voting time for the proposal OR the current value of `lockedUntil` whichever is greater/further in the future.
- CanLockStake -> Proposal Factory
- ProposalState -> IndividualProposal (increase vote by total dana in $DANA token UTXO, add user's address to `ProposalState.voterList`)



## Admin Minting Policy
Parameters:
```haskell
AdminMintingParams { targetCurrency :: ByteString -- fiat currency identifier
                   , adminStateCurrencySymbol :: CurrencySymbol
                   , adminValidatorAddress :: ValidatorHash
                   }
```

Purpose: The Admin Minting Policy mints various tokens used primarily by the Admin and Vault Validators.

### VaultRecord tokens (1 per vault)
  Purpose: - A record token to witness the creation of a vault within the Admin Validator such that vaults can easily be queried off-chain, or otherwise managed and confirmed to be valid.

  carries datum:
  ```haskell
  VaultRecord
    { userAddress :: Address
    , scriptAddress :: Address
    , collateralCurrency :: AssetClass
    }
  ```
  initialized to:
  ```haskell
  VaultRecord
    { userAddress = InitVaultAct.address
    , scriptAddress = scriptAddress
    , collateralCurrency = assetClass
    -- values determined by Validator
    }
  ```

  mint & burn: requires an AdminState token, which can be identified with the `AdminMintingParams.adminStateCurrencySymbol`

inputs:
- fee/collateral UTXO (from USER)
- AdminState token (from Admin Validator)

outputs:
- fee/collateral UTXO remainder -> user Wallet
- AdminState token -> Admin Validator
- VaultRecord token -> Admin Validator

### VaultState token (1 per vault)
  Purpose: A state token for Vault scripts,  a local copy of the relevant data from AdminState, stored in the Vault to avoid bottlenecks on the AdminState token.

  carries datum:
  ```haskell
  VaultState
  { collateralCurrency :: AssetClass
  , collateral :: Integer,
  , borrowPrincipal :: Integer
  , interestAccrued :: Integer
  , lastStabilityFeeTime :: PosixTime
  }
  ```
  Initialized to
  ```haskell
  VaultState
  { collateralCurrency = InitVaultAct.collateralCurrency
  , collateral = 0
  , borrowPrincipal = 0
  , interestAccrued = 0
  , lastStabilityFeeTime = currentTime
  }
  ```
  mint & burn: must contain the AdminState token.

inputs:
- fee/collateral UTXO (from USER)
- AdminState token (from Admin Validator)

outputs:
- fee/collateral UTXO remainder -> user Wallet
- AdminState token -> Admin Validator
- VaultState token -> new Vault Validator

## Admin Validator
Parameters:
```haskell
AdminValidatorParams
  { targetCurrency :: ByteString -- fiat currency identifier
  , oracleAddress :: ValidatorHash
  , oracleOperator :: PubKeyHash
  , treasuryAddress :: ValidatorHash
  , treasuryCurrencySymbol :: CurrencySymbol
  , adminStateCurrencySymbol :: CurrencySymbol
  , adminOperator :: PubKeyHash
  , ownerAddress :: PubKeyHash
  }
```

Purpose: the Admin Validator serves as a hub between all vaults, and serves to maintain the vault configuration as well as vault initialization.

Note that in the future (v2) the Admin Validator functions may instead be performed by a Proposal Factory and a much smaller script used exclusively for vault initialization.

Datum:
`AdminState`

Redeemer Type:
```haskell
= InitVaultAct { collateralCurrency :: AssetClass
               , address :: Address
               }
| UpdateAdminStateAct { adminState :: AdminState }
| RefreshAdminStateAct
| InitiateUpgradeAct { newContract :: ValidatorHash }
```

### InitVaultAct
Purpose: Initializes a vault in both the Admin Validator, and new Vault Validator script address.

Validation Rules:
- transaction must be signed by `InitVaultAct.address` or otherwise validated if it is a validatorHash.

note: the accompanying offchain code will instantiate a new vault validator script with the Admin Validator Script's `targetCurrency` parameters, and the InitVaultAct parameters, if this address already exists, we should fail the transaction - need a good way to test for this.

inputs:
- fee/collateral UTXO (from USER)
- AdminState token (from Admin Validator Script)

outputs:
- fee/collateral UTXO remainder -> user Wallet
- AdminState token -> Admin Validator Script (unchanged)
- VaultRecord Token (MINTED) -> Admin Validator Script
- VaultState Token (MINTED) -> New Vault Validator Script (`collateral`, `borrowPrincipal`, `stabilityFeeAccrued` fields should be initiated to zero, `lastStabilityFeeTime` initiated to currentTime)
- CanMintOrSpend Token (MINTED) -> New Vault Validator Script

### UpdateAdminStateAct
Purpose: Allows the `ownerAddress` to modify critical configuration in realtime without redeploying scripts.

this can also be used to disable all, or a subset of vaults in an emergency situation

Validation rules:
- must be signed by the `ownerAddress`
- all supported collateral types must have oracle support

*known issue: in order to service a massive number of vaults, a parallelized data copy strategy might be needed here*

inputs:
- fee/collateral UTXO (from USER)
- AdminState token (from Admin Validator Script)

outputs:
- fee/collateral UTXO remainder -> user Wallet
- AdminState token -> Admin Validator Script (updated by UpdateAdminStateAct fields and lastUpdated = currentTime, the `config` field only needs the `VaultConfig` relevant to the `InitVaultAct.collateralCurrency`)

### RefreshAdminStateAct
Purpose: allows a trusted automated bot to refresh the admin state without updating it so it does not go stale without the `ownerAddress`

Validation Rules: Must be signed by the `adminOperator`

otherwise implementation details should mirror `UpdateAdminStateAct` redeemer.

### InitiateUpgradeAct
Purpose: Triggers Upgrade procedure by securely sending an UpgradeContractToken to the Treasury.

note: this must be triggered in tandem with the InitiateUpgradeAct for the Treasury Validator.

Validation Rules:
- must be signed by the `ownerAddress`
- must include UpgradeContractToken from `treasuryAddress`.
- must set all supported assets to Disabled

inputs:
- fee/collateral UTXO (from USER)
- AdminState token (from Admin Validator Script)
- UpgradeContract token (from Admin Validator Script)
- UpgradeContract token (from Treasury Validator Script)

outputs:
- fee/collateral UTXO remainder -> user Wallet
- AdminState token -> Admin Validator Script (updated by UpdateAdminStateAct fields and lastUpdated = currentTime)
- UpgradeContract token -> Treasury Validator script (set `UpgradeContract.newContract` to `Just InitiateUpgradeAct.newContract`)
- UpgradeContract token -> `InitiateUpgradeAct.newContract` address (set `newContract` to `Nothing`)

## Vault Validator
parameters:
```haskell
VaultValidatorParams
  { targetCurrency :: ByteString
  , user :: Address
  , collateralCurrency :: AssetClass
  , adminStateCurrencySymbol :: CurrencySymbol
  , adminMintingCurrencySymbol :: CurrencySymbol
  , adminValidatorAddress :: ValidatorHash
  , oracleOperator :: PubKey
  , adminOperator :: PubKey
  }
  ```

Purpose: The vault manages interactions with a single `user` on a single `collateralCurrency`, this allows the stablecoin protocol to avoid major bottlenecks on the most common kinds of interactions: depositing & withdrawing collateral, borrowing and repaying stablecoin ($dUSD).

Scope notes:
the Vault has access to a constant, `vaultStateExpiryTime`, which should initially be set to 15 minutes (PosixTimeRange).

this is also used to check for the expiration of oracle info as well.

datum:
```haskell
VaultState
```
defined in _Admin Minting Policy_

Redeemer:
```haskell
= AddCollateralAct
   { amount :: Integer
   , adminState :: SignedMessage AdminState
   }
| RemoveCollateralAct
   { amount :: Integer
   , oraclePrice :: SignedMessage PriceTracking
   , adminState :: SignedMessage AdminState
   }
| AddBorrowAct
   { amount :: Integer
   , oraclePrice :: SignedMessage PriceTracking
   , adminState :: SignedMessage AdminState
   }
| RepayBorrowAct
   { amount :: Integer
   , oraclePrice :: SignedMessage PriceTracking
   , adminState :: SignedMessage AdminState
   }
| UpdateStabilityFeeAct
   { oraclePrice :: SignedMessage PriceTracking
   , adminState :: SignedMessage AdminState
   }
| LiquidationCallAct
   { oraclePrice :: SignedMessage PriceTracking
   , adminState :: SignedMessage AdminState
   , amount :: Integer
   }
```

`PriceTracking` is defined as follows:
```haskell
data PriceTracking = PriceTracking
  { -- | fiat currency id to price in lovelace
    fiatPriceFeed :: AssocMap ByteString Integer,
    cryptoPriceFeed :: AssocMap AssetClass Integer,
    lastUpdate :: PosixTime
  }
```

Scope Notes: Vaults have access to a constant `stabilityFeeBaseRate :: Rational`

### AddCollateralAct
Purpose: allow the user to deposit collateral into the system.

Validation Rules:
- AdminState must not be expired, AdminState Signature must be validated with `adminOperator`
- user must have provided a total of `AddCollateralAct.amount` across all collateral utxos

if the user has a previous UTXO with collateral, it should be consolidated
user may provide collateral in 1 or more seperate UTXOs

inputs:
- fee/collateral UTXO (from USER)
- VaultState token (from Vault Validator Script)
- Collateral UTXO (1 or more) (from USER)
- Optional: pre-existing collateral UTXO (from Vault Validator Script)

outputs:
- fee/collateral UTXO remainder -> user Wallet
- VaultState token -> Vault Validator script (increment `collateral` by `AddCollateralAct.amount`)
- collateral UTXO -> Vault validator Script (consolidated from above collateral UTXOs)


### RemoveCollateralAct
Purpose: Allow the user to withdraw collateral, if doing so does not put the Vault under the `minimumCollateralRatio`

Validation Rules:
- transaction must be validated by the user (tx signature or utxo included depending on address type)
- AdminState must not be expired, AdminState Signature must be validated with `adminOperator`
- PriceTracking data must not have expired
- PriceTracking data must be signed by the `oracleOperator`
- Stability fees must be applied, if there is a previous borrow amount
- the resulting `VaultState` after decrementing `VaultState.collateral` and applying stabilitiy fees must not be below the `minimumCollateralRatio`.

inputs:
- fee/collateral UTXO (from USER)
- VaultState token (from vault Validator Script)
- collateral UTXO (from Vault validator script)

outputs:
- fee/collateral UTXO remainder -> user Wallet
- VaultState token -> Vault Validator script (decrement `VaultState.collateral` by `RemoveCollateralAct.amount`, apply stability fees)
- Withdraw UTXO -> User Wallet (from collateral UTXO above)
- Optional collateral UTXO -> Vault validator script -- if all collateral is not withdrawn.

### AddBorrowAct
Purpose: Allow the user to borrow against collateral, if doing so does not put the Vault under the `minimumCollateralRatio`

Validation Rules:
- transaction must be validated by the user (tx signature or utxo included depending on address type)
- AdminState must not be expired, AdminState Signature must be validated with `adminOperator`
- PriceTracking data must not have expired
- PriceTracking data must be signed by the `oracleOperator`
- Stability fees must be applied, if there is a previous borrow amount
- The resulting `VaultState` after incrementing `VaultState.borrow` and applying stabilitiy fees must not be below the `minimumCollateralRatio`.

inputs:
- fee/collateral UTXO (from USER)
- VaultState token (from vault Validator Script)
- CanMintOrSpend (from Vault Validator Script)

outputs:
- fee/collateral UTXO remainder -> user Wallet
- VaultState token -> Vault Validator script (increment borrowPrinciple by AddBorrowAct.amount)
- CanMintOrSpend -> Vault Validator Script
- $dUSD (MINTED) -> user wallet

### RepayBorrowAct
Purpose: Allow the user to repay a borrow

Validation Rules:
- transaction must be validated by the user (tx signature or utxo included depending on address type)
- AdminState must not be expired, AdminState Signature must be validated with `adminOperator`
- PriceTracking data must not have expired
- PriceTracking data must be signed by the `oracleOperator`
- Stability fees must be applied.
- the user may include 1 or more $dUSD Payment UTXOs
- payment is issued against `stabilityFeeAccrued` first, then `borrowPrincipal`

inputs:
- fee/collateral UTXO (from USER)
- VaultState token (from vault Validator Script)
- CanMintOrSpend (from Vault Validator Script)
- $dUSD payment UTXOs (from USER)

note: the 'portions' below are yet to be determined, tentatively fees paid against accruedStabilityFees will be split 50/50 between the `ProvideRewardAct` and the private `ownerAddress`.

outputs:
- fee/collateral UTXO remainder -> user Wallet
- VaultState token -> Vault Validator script (decrement borrowPrinciple, interestAccrued by AddBorrowAct.amount)
- CanMintOrSpend -> Vault Validator Script
- Optional $dUSD against principal (BURN) (only if RepayBorrowAct.amount is greater than stability fees applied to the principal)
- $dUSD against accruedStabilityFees -> Governance Validator Address ProvideRewardAct (portion)
- $dUSD against accruedStabilityFees -> `ownerAddress` (portion)
- $dUSD remainder -> User Wallet (only if provided amount exceeds RepayBorrowAct.amount)

### UpdateStabilityFeeAct
Purpose: Apply Stability fees (interest) to a vault, may be executed on it's own, or as a procedure to verify other actions (AddBorrowAct, RepayBorrowAct, RemoveCollateralAct, LiquidationCallAct)

Validation Rules:
- AdminState must not be expired, AdminState Signature must be validated with `adminOperator`
- PriceTracking data must not have expired
- PriceTracking data must be signed by the `oracleOperator`
- Stability fees must be applied.
- Stability fee calculation is verified on chain.
- (`VaultState.stabilityFeeMultiplier` * stabilityFeeBaseRate) is used to calculate the stability fee against both principal and any previously accruedStabilityFees,

inputs:
- fee/collateral UTXO (from USER)
- VaultState token (from vault Validator Script)

outputs:
- fee/collateral UTXO remainder -> user Wallet
- VaultState token -> Vault Validator script (increment stabilityFeeAccrued by stabilityFee)

### LiquidationCallAct
Purpose: must happen in tandem with a LiquidationPurchaseAct on the Buffer, this validates a liquidation state within the vault

Validation Rules:
- AdminState must not be expired, AdminState Signature must be validated with `adminOperator`
- PriceTracking data must not have expired
- PriceTracking data must be signed by the `oracleOperator`
- Stability fees must be applied.
- the resulting VaultState (after adjusting for pricetracking data, applying stability fees.) must have _otherwise_ been under the `minimumCollateralRatio`, without adjusting for the liquidation payment.
- prevent total liquidation if a partial liquidation is viable

note: the user may supply 1 or more Liquidation Payment utxos

inputs:
- fee/collateral UTXO (from USER)
- VaultState token (from Vault Validator Script, expired)
- Collateral UTXO (from Vault Validator Script)
- Liquidation payment $dUSD UTXO (1 or more) (from USER)

note: the 'portions' below are yet to be determined, tentatively fees paid against accruedStabilityFees will be split 50/50 between the `ProvideRewardAct` and the private `ownerAddress`

outputs:
- fee/collateral UTXO remainder -> user Wallet
- VaultState token -> Vault Validator script (updated to match Admin copy)
- Optional $dUSD against principal (BURN) (from Liquidation payment above)
- $dUSD against accruedStabilityFees -> governanceScriptAddress (portion) (from Liquidation payment above) -- ProvideRewards
- $dUSD against accruedStabilityFees -> owner address (portion) (from Liquidation payment above)
- Optional Liquidation payment remainder UTXO -> User Wallet (if `LiquidationCallAct.amount` less than the total $dUSD liquidation payment provided by the user
- Collateral UTXO -> User wallet

## Buffer Monetary Policy
Purpose: Mints state & record tokens relating to the buffer, used to determine auction prices.

-- this token may be moved to _One Shot Tokens_
### BufferState token (1x per supported collateral currency)
  Token Type: State token for Buffer Validator
  carried datum:
  ```haskell
  BufferState
    { currentDiscount :: Ratio -- discount for Liquidation
    , debtPrice :: Maybe Integer -- current price for debt auction, if any
    , surplusPrice :: Maybe Integer -- current price for surplus auction, if any
    }
  ```
  initiated to:
  ```haskell
  BufferState
    { currentDiscount = 1 % 5
    , debtPrice = oracleMessageDebtPrice
    , surplusPrice = oracleMessageSurplusPrice
    }
  ```
*known issue: we should break the `currentDiscount` into a distributed mapping from assetClass to Ratio, since different currencies may have prices that move independently.

*known issue: how do we track the un-backed $dUSD? or amount of outstanding liquidatable collateral? we likely need to add record tokens*

## Buffer Validator
Parameters:
```haskell
BufferValidatorParams
  { oracleAddress :: ValidatorHash
  , oracleOperator :: PubKeyHash
  , peggedCurrency :: ByteString -- Fiat currency identifier
  }
  ```

Purpose: To orchestrate Ardana-dollar's 3 auctions:
- Liquidation (Collateral for $dUSD)
- Debt ($DANA for dUSD)
- Surplus ($dUSD for $DANA)

Datum:
`BufferState`

Redeemer
```haskell
= LiquidationPurchaseAct
    { amount :: Integer
    , priceTracking :: SignedMessage PriceTracking
    }
| DebtPurchaseAct
| SurplusPurchaseAct
```

Scope notes:  the Buffer Validator has access to the `vaultStateExpiryTime` constant, which it uses to validate Pricetracking info

*known issue: we need to expand the debt and surplus types here*

### LiquidationPurchaseAct
Purpose: to validate pricing-specific data related to a Liquidation. should be called in tandem with LiquidationCallAct in the relevant Vault Validator.

note: off chain code should search for vaults that can be liquidated, on chain code only needs proof that a particular vault is able to be liquidated.

*known issue: how do we effectively seize collateral upfront, rather than liquidating in one step? can someone exploit this system to prevent being liquidated? in a price-collapse event, how can we make sure we always have liquidators?*


Validation rules:
- price must match the `BufferState.currentDiscount`, when added to the `LiquidationPurchaseAct.priceTracking`
- fail if PriceTracking data is expired
- fail if PriceTracking data is not signed by the Oracle Operator
- payment is applied against `stabilityFeeAccrued` first before `borrowPrinciple`
- the user may provide one or more liquidation payment UTXO containing $dUSD
- prevent total liquidation if a partial liquidation is viable

inputs:
- fee/collateral UTXO (from USER)
- VaultState token (from Vault Validator Script, expired)
- Collateral UTXO (from Vault Validator Script)
- Liquidation payment $dUSD UTXO (1 or more) (from USER)

note: the 'portions' below are yet to be determined, tentatively fees paid against accruedStabilityFees will be split 50/50 between the `ProvideRewardAct` and the private `ownerAddress`

outputs:
- fee/collateral UTXO remainder -> user Wallet
- VaultState token -> Vault Validator script (updated to match Admin copy)
- Optional $dUSD against principal (BURN) (from Liquidation payment above)
- $dUSD against accruedStabilityFees -> governanceScriptAddress (portion) (from Liquidation payment above) -- ProvideRewards
- $dUSD against accruedStabilityFees -> owner address (portion) (from Liquidation payment above)
- Optional Liquidation payment remainder UTXO -> User Wallet (if `LiquidationCallAct.amount` less than the total $dUSD liquidation payment provided by the user
- Collateral UTXO -> User wallet


### DebtPurchaseAct

-- TODO
### SurplusPurchaseAct
-- TODO

## Treasury Minting Policy
Parameters:
```haskell
TreasuryMintingParams
  { initialOwner :: PubKeyHash
  , peggedCurrency :: ByteString
  }
  ```

Purpose: The TreasuryMintingPolicy allows us to keep a consistent currency symbol for $dUSD across multiple versions of the protocol, Additionally The Treasury Minting Policy mints tokens used in Upgrade and other security-sensitive procedures.

Note: the treasury scripts will not be updated as part of version 2 or any future version of the protocol, Instead, the treasury will integrate with those future versions of the protocol without any code changes.

note: though `dUSD` is used below,  this is in fact a parameter-dependant token name based on `"d" <> Parameters.peggedCurrency`, by adjusting this and corresponding parameters throughout the protocol, we can easily create dGBP etc.

### UpgradeContract Token Minting (2 in existence for all time)
Purpose: This is a State Token for the Treasury, it is also a Permission token to upgrade the contract

-- TODO - these concerns should probably be divided into 2 seperate tokens, do these constutute one-shot tokens?

  carries Datum:
  ```haskell
  UpgradeContract
   { newContract :: Maybe Address }
   ```
  initiated to:
  ```haskell
  UpgradeContract
   { newContract :: Just v1AdminContractAddress }
   ```
Minting
- must be signed by the `initialOwner`
- datum always Initially set to Nothing
- this will be called directly, and should only be called exactly once in the lifetime of the script, to mint 2 UpgradeContract tokens in 2 seperate UTXOs, once this is completed, any attempt to mint this token should fail

Burning: this token cannot be burned.

inputs:
- fee/collateral UTXO (from USER)

outputs:
- UpgradeContract token -> Treasury Validator (set newContract to `Just (Admin Contract Address)`)
- UpgradeContract token -> (newContract Admin Contract Address) (set newContract to `Nothing`)

### $dUSD Token
Purpose: The protocol's main product/service, a stablecoin pegged to the US Dollar

carries no datum

note: currently there is not a way to enforce that only CanMintOrSpend minted _for_ the most recent scripts can be used to mint $dUSD, without relying on the UpgradeContract token held by the Treasury Validator Script, therefore it is imperative that the Admin Validator disable all child-Scripts (vault, buffer) permanently once the upgrade has occured, otherwise we open ourselves up to minting for different versions of the protocol, which should be avoided.

Minting & Burning:
- must contain a `CanMintOrSpend` token from a script (scriptRequestingMint)
- Burning can always succeed

inputs:
- fee/collateral UTXO (from USER)
- CanMintOrSpend token (from scriptRequestingMint)

outputs:
- fee/collateral UTXO remainder -> user Wallet
- CanMintOrSpend token -> scriptRequestingMint
- $dUSD (Minted) -> (address determined by scriptRequestingMint)

### CanMintOrSpend Token Minting
Purpose: A Permission token which signifies permission to Mint $dUSD, or spend from a treasury Cost center. This is used when Initializing a new Vault, or during Upgrade bootstrapping to allow a Buffer contract to mint or spend tokens from the Treasury.

carries no datum

-- note: we should consider splitting this token into two different tokens to make permissions more granular. perhaps the VaultRecord token can be useful here.
Vaults should not have the right to spend,   additionally it would be _ideal_ if we did not need extra permission tokens for the vaults, however this may harm upgradeability.

Minting & Burning:
- the UpgradeContract token from Treasury Validator Script will have a `newContract` value with an address attached to it, the second UpgradeContract token must be from that `newContract` address.

inputs:
- fee/collateral UTXO (from USER)
- UpgradeContract token (from Treasury Validator Script)
- UpgradeContract token UTXO from newContract Address

output:
- fee/collateral UTXO remainder -> user Wallet
- UpgradeContract token -> Treasury Validator Script (unchanged)
- UpgradeContract token UTXO -> newContract Address
- CanMintOrSpend token -> (address determined by newContract Validator)

*known issue: when exactly does this get minted for Admin/Buffer?*

### CostCenterState token (1x for each CostCenter)
  Purpose: a State Token for a cost center, To carry the balance of a particular Cost Center (an account) of funds that may be held in the Treasury and spent with permission. This prevents accidental overspending of one set of funds, depleting funds needed for other purposes.

  carries datum:
  ```haskell
  CostCenterState
    { costCenter :: ByteString
    , balance :: Plutus.Value
    }
  ```

Minting & Burning:
- Must contain an UpgradeContract token utxo from the Treasury Validator

inputs:
- fee/collateral UTXO (from USER)
- UpgradeContract token (from Treasury Validator Script)

output:
- fee/collateral UTXO remainder -> user Wallet
- UpgradeContract token -> Treasury Validator Script (unchanged)
- CostCenterState token -> Treasury Validator Script (values depend on treasury validator)


## Treasury Validator
Parameters:
```haskell
TreasuryValidatorParams
  { TreasuryMintingCurrencySymbol :: CurrencySymbol
  , danaTokenAssetClass :: AssetClass
  }
```
Purpose: The Treasury is a structure that allows for a store of funds across multiple versions of the protocol, totals of these funds are stored in `CostCenterState` datum.

Note: the treasury scripts will not be updated as part of version 2 or any future version of the protocol, Instead, the treasury will integrate with those future versions of the protocol without any code changes.

datum:
`UpgradeContract`

redeemer
```haskell
= InitiateUpgradeAct { newContract :: ValidatorHash }
| SpendFromCostCenterAct { value :: Plutus.Value, costCenter :: ByteString, beneficiary :: Address }
| DepositFundsWithCostCenterAct { value :: Plutus.Value, costCenter :: ByteString }
```

### InitiateUpgradeAct
Purpose: To validate an upgrade procedure, and to record the new contract so that trust on spending, minting, protocol updates, and other sensitive actions is directed correctly.

this should be called in tandem with Admin Validator's `InitiateUpgradeAct`

Validation Rules:
- the transaction must include an UpgradeContract token from the Treasury Validator script, this must specify a `newContract` which must be a `Just Address` this address will be the currentContract`
- the transaction must include an UpgradeContract token from the previous `newContract` address
- the AdminState datum must be rewritten to disable all currencies

*known issues: Requiring AdminState here will force us to a particular AdminState type in the future, this should be validated in the Admin Validator*

inputs:
- fee/collateral UTXO (from USER)
- AdminState token (from Admin Validator Script)
- UpgradeContract token (from Admin Validator Script)
- UpgradeContract token (from Treasury Validator Script)

outputs:
- fee/collateral UTXO remainder -> user Wallet
- AdminState token -> Admin Validator Script (update `VaultConfig` fields to disable all currencies, and `lastUpdated = currentTime`)
- UpgradeContract token -> Treasury Validator script (set `UpgradeContract.newContract` to `Just InitiateUpgradeAct.newContract`
UpgradeContract token -> `InitiateUpgradeAct.address` (set `UpgradeContract.newContract` to `Nothing`)

### SpendFromCostCenterAct
Purpose: To allow the protocol to spend stored funds, for example, as part of a Surplus Auction

Validation Rules:
- if no token is supplied with `costCenter` matching redeemer values, fail
- if `SpendFromCostCenterAct.value` requires more of any token than is available in the relevant `CostCenterState`, fail

inputs:
- fee/collateral UTXO (from USER)
- CostCenterState token (from Treasury Validator Script)
- payment UTXO (from Treasury Validator Script)

outputs:
- fee/collateral UTXO remainder -> user Wallet
- CostCenterState token -> Treasury Validator Script, decrement`value` based on redeemer
- payment UTXO -> SpendFromCostCenterAct.beneficiary address

### DepositFundsWithCostCenterAct
Purpose: To allow users or the protocol to deposit funds to a particular cost center, for later use.
Validation Rules:
- if a UTXO exists in the Treasury Validator for the given `costCenter`, then it must be included, otherwise one is minted
- user may include one or more UTXOs as payment

inputs:
- fee/collateral UTXO (from USER)
- Optional CostCenterState token (from Treasury Validator Script)
- payment UTXO (from USER)

outputs:
- fee/collateral UTXO remainder -> user Wallet
- CostCenterState token (MINTED?) -> Treasury Validator Script, set `costCenter` and `value` based on redeemer
- payment UTXO -> Treasury Validator Script (consolidated by `costCenter`)

