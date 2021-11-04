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
```
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
  ```
  GovernanceState { stakeTotal :: Integer
                  , rewardsTotal :: Value
                  , lastEpoch :: PosixTime
                  }
  ```

  initialized to
  ```
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

## Governance Minting Policy
parameters:
```
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
  ```
  UserStakeDetail { userAddress :: Address
                  , amountStaked :: Integer
                  , lastReward :: PosixTime
                  , lockedUntil :: Maybe PosixTime
                  , lockedBy :: Maybe Address
                  }
  ```

  initialized to
  ```
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
  ```
  LastEpochScriptState { stakeTotal :: Integer
                       , rewardsTotal :: Value
                       }
  ```

  initialized to
  ```
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
```
GovernanceValidatorParams { governanceStateCurrencySymbol :: CurrencySymbol
                          , ownerAddress :: PubKeyHash
                          , danaTokenAssetClass :: AssetClass
                          }
```

Datum:
```
GovernanceState
```
defined as a State token datum under _One Shot Tokens_

Redeemer:
```
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
```
ProposalMintingParams { proposalFactoryAddress :: Address
                      , governanceMintingCurrencySymbol :: CurrencySymbol
                      }
```

Purpose: The Proposal Minting Policy manages state tokens relating to a hypothetical Proposal Factory and/or IndividualProposal script.

note: this will not actually be implemented, it's intended as a guide for those who might need to integrate with the Governance Validator.

### ProposalState Token
  Token Type: State token for IndividualProposal Validator script.

  Carries Datum:
  ```
  ProposalState
    { totalVote :: Integer
    , voterList :: [Address]
    }
  ```
  initialized to:
```
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
```
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
```
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
```
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
```
ProposalState
```
defined at _Proposal Minting Policy_

Redeemer:
```
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

## Buffer Monetary Policy
Purpose: Mints state & record tokens relating to the buffer, used to determine auction prices.

-- this token may be moved to _One Shot Tokens_
### BufferState token (1x per supported collateral currency)
  Token Type: State token for Buffer Validator
  carried datum:
  ```
  BufferState
    { currentDiscount :: Ratio -- discount for Liquidation
    , debtPrice :: Maybe Integer -- current price for debt auction, if any
    , surplusPrice :: Maybe Integer -- current price for surplus auction, if any
    }
  ```
  initiated to:
  ```
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
```
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
```
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

