# UTXO design

Where the endpoint-spec describes the Schema design, this document will describe the Validators, and Monetary Policy scripts that those endpoints will call.

This will cover all on-chain components necessary to implement the endpoint-spec.

-- TODO make sure all monetary policies have restrictions on both mint and burn conditions for all tokens
-- TODO make sure all validators have datum and redeemer types well defined
-- TODO revise endpoint spec to match script behaviors and list script actions that arise from each endpoint call.
-- TODO resolve oracle/admin bottleneck? (price token and vaultParams)
-- TODO how to track un-backed dUSD?
-- TODO negative number conditions
-- TODO liquidation seizure in advance
-- TODO debt and surplus auction
-- TODO dUSD Savings system
-- TODO - Governance MonetaryPolicy and Governance Validator should be named DANAStakePool
-- TODO remove confusing term 'contract'

## GovernanceState Monetary Policy
parameters: { governanceScriptAddress :: Address }

tokens minted: 
- userStakeDetail token (1 per user)
  carries datum: 
  UserStakeDetail { userAddress :: Address
                  , lastReward :: PosixTime 
                  , lockedUntil :: Maybe PosixTime
                  , lockedBy :: MaybeAddress
                  }
- StakePoolTotalState token (1 ever, unique)
  carries datum: 
  StakePoolTotals { stakeTotal: Integer, rewardsTotal: Value }
- LastEpochUserState token (1 per user, per epoch)
  carries datum:
  LastEpochUserState { address :: Address
                     , danaStakeAmt :: Integer
                     , epoch :: Integer
                     }
- LastEpochScriptState token (1 per epoch)
  carries datum: { stake total :: Integer, rewardsTotal :: Integer }
- CanLockStake token (1 per ProposalScript)
  (carries no datum)

minting predicates:
must contain a utxo from `governanceScriptAddress`
(return utxo to governanceScriptAddress)
this should cover all minting needs other than StakePoolTotalState described below

Burning Predicates: must contain a utxo from governanceScriptAddress


### StakePoolTotalState minting

mints a `stakePoolTotals` state token using GovernanceStateMonetaryPolicy
initialized to { stakeTotal = 0, rewardsTotal = Value.empty }

if there is already a stakePoolTotals state token in the governance validator script, fail

inputs:
fee/collateral UTXO

outputs:
stakePoolTotals state token -> Governance Validator Script

## Governance Validator
parameters: none

Datum: Maybe (UserStakeDetails { userAddress :: Address
                        , lastReward :: PosixTime 
                        , lockedUntil :: Maybe PosixTime
                        , lockedBy :: Maybe Address
                        })
Redeemer: DepositAct { amount :: Integer
                     , address :: Address 
                     }
        | WithdrawAct { amount :: Integer
                      , address :: Address 
                      }
        | ProvideRewardAct { value :: PlutusTx.Value }
        | ClaimRewardAct { address :: Address
                         , epoch :: Integer }
        | FinishRewardsForEpochAct { epoch :: Integer }
        | RegisterProposalScriptAct { scriptAddress :: Address }
        | LockForVotingAct { voterAddress :: Address
                           , lockedUntil :: Integer}
        | TriggerEpochAct
        | TriggerEpochUserAct

The Governance Validator script also carries the notion of an `owner` address, this will be a hardcoded constant address used for registering new ProposalScripts.
ProposalScripts will be modular scripts which interact with the Governance Validator script for the purpose of collecting voter sentiment on a specific type of issue. They may lock and unlock user stake totals to prevent vote duplication.

### DepositAct
- user at `address` must have provided 1 or more UTXO's containing a total of at least `amount` of DANA (Deposit UTXOS)
- user at `address` must have signed the transaction.
- optional - user may have included a UserStakeDetail from the Governance validator script matching their `address`, if not, then one should be Minted from `GovernanceStateMonetaryPolicy`, with the user's `address` as the `address` parameter, currentTime as `lastReward` parameter, and `Nothing` as the `lockedUntil` and `lockedBy` parameters, and returned to the governance validator script address.
- if there is no stakePoolTotals token in the Governance Validator script, Fail

inputs:
fee/collateral UTXO (from USER)
deposit utxos (1 or more) (from USER)
stakePoolTotals state token  (from Governance Validator Script)
(optional userStakeDetail Token) (from Governance Validator Script)

outputs:
fee/collateral UTXO remainder -> user Wallet
deposit UTXOs -> Governance Validator script 
stakePoolTotals state token -> Governance Validator script (increment total stake amount by deposit amount)
userStakeDetail Token (MINTED?) -> Governance Validator script (increment by deposit amount OR mint fresh with deposit amount)

### WithdrawAct
- if UserStakeDetail datum is Nothing and/or and userStakeDetail token UTXO is not provided as input, or if there is any mismatch in the data provided, fail
- if amount specified in WithdrawAct.amount is less than the balance of the UserStakeDetail, fail
- transaction must be signed by WithdrawAct.Address
- if UserStakeDetail.lockedUntil is a future PosixTime, fail.

inputs:
fee/collateral UTXO (from USER)
stakePoolTotals state token  (from Governance Validator Script)
userStakeDetail state token  (from Governance Validator Script)

outputs:
fee/collateral UTXO remainder -> user Wallet
stakePoolTotals state token -> Governance Validator script (decrement total stake amount by WithdrawAct.amount)
userStakeDetail Token -> Governance Validator script (decrement by deposit amount)
withdrawal UTXO (contains WithdrawAct.amount of Dana) -> WithdrawAct.Address

### ProvideRewardAct
- user may include one or more UTXOs with rewards in a Value, (reward UTXOs) these should total to match ProvideRewardsAct.value
- disregard userStakeDetail when validating this action.
- user must have signed the transaction

inputs:
fee/collateral UTXO (from USER)
reward UTXOs (from USER)
stakePoolTotals state token (from Governance Validator Script)
optional previous reward UTXO for this epoch (from Governance Validator script)

outputs:
fee/collateral UTXO remainder -> user Wallet
stakePoolTotals state token -> Governance Validator script (increment rewardsTotal by ProvideRewardsAct.value)
reward UTXOs -> Governance Validator Script (consolidate to single utxo per epoch)

### ClaimRewardAct
- the user may include 1 UTXO containing rewards, this is a consolidated reward UTXO 
- ClaimRewardAct.address must sign the transaction

inputs:
fee/collateral UTXO (from USER)
LastEpochScriptState (from Governance Validator Script)
LastEpochUserState (for ClaimRewardAct.address and ClaimRewardAct.epoch, from Governance validator script)
reward UTXO (from Governance Validator Script)

outputs:
fee/collateral UTXO remainder -> user Wallet
LastEpochScriptState -> Governance Validator Script
LastEpochUserState -> BURN
reward UTXO -> Governance Validator Script
reward UTXO -> User wallet

### FinishRewardsForEpochAct
- `owner` address must sign this transaction
- moves any remaining reward funds from FinishRewardsForEpochAct.epoch to the current epoch.

inputs:
fee/collateral UTXO (from USER)
LastEpochScriptState (from Governance Validator Script)
reward UTXO (for FinishRewardsForEpochAct.epoch) -> Governance Validator Script
reward UTXO (for current epoch)

outputs:
fee/collateral UTXO remainder -> user Wallet
LastEpochScriptState -> BURN
reward UTXO -> Governance Validator Script (consolidated)

### RegisterProposalScriptAct
- `owner` address must sign this transaction

inputs:
fee/collateral UTXO (from USER)

outputs:
fee/collateral UTXO remainder -> user Wallet
canLockStake token (MINTED) -> RegisterProposalScriptAct.scriptAddress

### LockForVotingAct
- LockForVotingAct.voterAddress must have signed this transaction

inputs:
fee/collateral UTXO (from USER)
CanLockStake token (from ProposalScript)
UserStakeDetail (for LockForVotingAct.voterAddress, from Governance Validator Script)

outputs:
fee/collateral UTXO remainder -> user Wallet
canLockStake token (Return to ProposalScript)
UserStakeDetail -> Governance Validator Script (set lockedUntil = LockForVotingAct.lockedUntil, set lockedBy = ProposalScript address) 

### TriggerEpochAct
- this requires knowledge of Cardano epochs on chain
- if the Governance Validator Script has LastEpochScriptState token which was minted in this epoch, fail
- otherwise the transaction should mint a LastEpochScriptState token, which is a copy of the current `stakePoolTotalState`, along with the epoch number that has passed (each epoch, one of these tokens can be minted for the previous epoch)
- disregard userStakeDetail when validating this action.

can be called by anyone

inputs:
fee/collateral UTXO remainder (from USER)
stakePoolTotals state token (from Governance Validator script)

outputs:
fee/collateral UTXO remainder -> user Wallet
stakePoolTotals state token -> Governance Validator script 
LastEpochScriptState state token (MINTED)-> Governance Validator Script

### TriggerEpochUserAct
- this requires knowledge of Cardano epochs on chain
- if the Governance Validator Script has a LastEpochUserState token for the current user which was minted in this epoch, fail
- otherwise the transaction should mint a LastEpochUserState token, which is a copy of the current UserStakeDetail for the given user

inputs:
fee/collateral UTXO (from USER)

outputs:
fee/collateral UTXO remainder -> user Wallet
LastEpochUserState state token (MINTED)-> Governance Validator Script

## Proposal Monetary Policy (example)
parameters: { proposalScript :: Address }

tokens minted
ProposalState (1x per individual proposal)
carries datum:
 { totalVote :: Integer
 , voterList :: [Address]]
 } -- this data may vary depending on vote counting mechanism

validation rules
- must include an arbitrary utxo from the `proposalScript` address

## ProposalScript (Example)
Parameters: { govAddress :: Address } -- we may add other script addresses to facilitate propsal execution

A ProposalScript will integrate with the Governance script directly, providing the structure and logic to make a governance decision on a particular kind of question.
Since this is an example, certain details described on other parts of the spec are not available at this time. 

This is an example for any service (Ardana Dex, Ardana-Dollar v2) that may perform an integration with the Governance Validator script

A ProposalScript is more of a factory for individual proposals, and it will carry the CanLockStake token used to prevent vote exploits

A Proposal Script should carry specific logic pretaining to success conditions, quorum, minimum proposal requirements, the data which needs approval, and the voting period time limits for a proposal type, as well as any effects necessary to implement the change described (such as updating a contract configuration).


### CreateProposalAct
this would create a new IndividualProposal validator with supplied parameters (off chain) which carries data for approval as a parameter.
- user must have more DANA than the minimum proposal requirement.

inputs:
fee/collateral UTXO (from USER)
DANA token utxos for user (from Governance Validator script)

outputs:
fee/collateral UTXO remainder -> user Wallet
DANA token utxos for user -> Governance Validator Script
ProposalState token (MINTED)-> IndividualPropsal Validator Script

## IndividualPropsal (Example)
Parameters: (some specific data used in democratic action)

### VoteAct
- user must not be on `voterList` 
- user must sign transaction

inputs:
fee/collateral UTXO (from USER)
DANA token utxos for user (from Governance Validator script)
UserStakeDetail for user (from Governance Validator script)
CanLockStake (from ProposalScript)
ProposalState (from IndividualProposal)


outputs:
fee/collateral UTXO remainder -> user Wallet
DANA token utxos for user -> Governance Validator Script
UserStakeDetail -> Governance validator Script (set lockedBy to proposal Contract and `lockedUntil` to the end of the voting time for the proposal OR the current value of `lockedUntil` whichever is greater/further in the future.
CanLockStake -> ProposalScript
ProposalState -> IndividualProposal (increase vote by total dana in DANA token utxos, add user's address to voterList) 


## AdminState Monetary Policy
parameters: { targetCurrency :: ByteString } -- fiat currency identifier

Minted currencies:
- vaultRecord tokens (1 per vault)
  carries datum:
  VaultRecord 
  { userAddress :: Address
  , scriptAddress :: Address 
  , collateralCurrency :: AssetClass
  }
- vaultParams token (1x for Admin Validator, 1x for each vault)
  carries datum:
  VaultParams 
  { configs :: [(VaultConfig, Rational)], -- asset class and the default stability fee multiplier for dUSD loans backed by that asset class
  , liquidationBenefitCap :: Rational
  , liquidationBenefitFloor :: Rational
  , minCollateralRatio :: Rational
  , lastUpdated :: PosixTime
  , disabledAssets :: [AssetClass]
  }
- VaultState token
  carries datum:
    VaultState :: { collateral :: Integer, 
                  , borrowPrincipal :: Integer
                  , interestAccrued :: Integer
                  , lastStabilityFeeTime :: PosixTime
                  }

Predicates: must contain UTXO from Admin Validator script address


## Admin Validator
Parameters: { targetCurrency :: ByteString -- fiat currency identifier
            , oracleAddress :: Address
            , treasuryAddress :: Address
            }

note that in the future (v2) the Admin contract functions may instead be performed by a ProposalScript.


DatumType: ()
Redeemer Type:
  InitVaultAct { collateralCurrency :: AssetClass
               , address :: Address
               }
| UpdateVaultParamsAct { vaultParams :: VaultParams }
| EmergencyDisableAct { assetClasses :: [AssetClass] }
| InitiateUpgradeAct { newContract :: Address }

like the `Governance` Validator script, `Admin` has access to a hardcoded `owner` address (these values will be the same for both scripts 


### InitVaultAct
- transaction must be signed by InitVaultAct.address
- the VaultParams provided to the vault script is a copy of the VaultParams held by the Admin Validator script, with the `lastUpdated` time set to the currentPosixTime 

note: the accompanying offchain will instantiate a new vault validator script with the Admin Validator Script's `targetCurrency` parameters, and the InitVaultAct parameters, if this address already exists, we should fail the transaction.
- need a good way to test for this.

inputs:
fee/collateral UTXO (from USER)
VaultParams State token (from Admin Validator Script)

outputs:
fee/collateral UTXO remainder -> user Wallet
VaultParams State token -> Admin Validator Script (unchanged)
VaultRecord Token (MINTED) -> Admin Validator Script
VaultState Token (MINTED) -> New Vault Validator Script (collateral, borrowPrincipal, stabilityFeeAccrued should  be initiated to zero, lastStabilityFeeTime initiated to currentTime)
VaultParams State token (MINTED) -> New Vault  Validato Script (set lastUpdated to CurrentTime)

### UpdateVaultParamsAct
- must be signed by the `owner`
- If a VaultParams State token is held in the Admin Validator script, it must be provided as an input, otherwise it is minted.

inputs: 
fee/collateral UTXO (from USER)
optional VaultParams State token (from Admin Validator Script)

outputs:
fee/collateral UTXO remainder -> user Wallet
VaultParams State token (MINTED?)-> Admin Validator Script (updated by UpdateVaultParamsAct fields and lastUpdated = currentTime)

### InitiateUpgradeAct
- must be signed by the `owner`
- must set all supported assets to Disabled

inputs:
fee/collateral UTXO (from USER)
VaultParams State token (from Admin Validator Script)
UpgradeContract token (from Admin Validator Script)

outputs:
fee/collateral UTXO remainder -> user Wallet
VaultParams State token -> Admin Validator Script (updated by UpdateVaultParamsAct fields and lastUpdated = currentTime, disabledAssets = all supported asset types)
UpgradeContract token -> Treasury Validator script (set scriptAddress to InitiateUpgradeAct.newContract

## Vault Validator
parameters: { targetCurrency :: ByteString, 
            , user :: Address
            , collateralCurrency :: AssetClass
            }
the Vault has access to a constant, `vaultParamExpiryTime`, which should initially be set to 15 minutes (PosixTimeRange).
this is also used to check for the expiration of oracle info as well.

datum: VaultState
Redeemer:
= AddCollateralAct 
   { amount :: Integer
   }
| RemoveCollateralAct
   { amount :: Integer
   }
| AddBorrowAct
   { amount :: Integer
   }
| RepayBorrowAct
   { amount :: Integer
   }
| UpdateStabilityFeeAct
| UpdateVaultParamsAct
| LiquidationCallAct

### AddCollateralAct
- VaultParams must not have expired
- PriceTracking token must not have expired

if the user has a previous UTXO with collateral, it should be consolidated

inputs: 
fee/collateral UTXO (from USER)
VaultParams token (from Vault)
VaultState token (from vault
PriceTracking Oracle Token (from oracle Validator Script)
Optional: pre-existing collateral utxo


outputs:
fee/collateral UTXO remainder -> user Wallet
VaultParams token -> Vault Validator script
VaultState token -> Vault Validator script (increment `collateral` by AddCollateralAct.amount)
PriceTracking Oracle Token -> Oracle Script
collateral utxo -> Vault validator Script (consolidated)


### RemoveCollateralAct
- transaction must be signed by the user
- VaultParams must not have expired
- PriceTracking token must not have expired
- the resulting VaultState after decrementing `collateral` must not be below the minimumCollateralRatio

inputs: 
fee/collateral UTXO (from USER)
VaultParams token (from Vault Validator Script)
VaultState token (from vault Validator Script)
PriceTracking Oracle Token (from oracle Validator Script)
collateral UTXO (from Vault validator script)

outputs:
fee/collateral UTXO remainder -> user Wallet
VaultParams token -> Vault Validator script
VaultState token -> Vault Validator script (decrement `collateral` by RemoveCollateralAct.amount)
PriceTracking Oracle Token -> Oracle Script
withdraw UTXO -> User Wallet
Optional collateral UTXO -> Vault validator script -- if all collateral is not withdrawn.

### AddBorrowAct
- transaction must be signed by the user
- VaultParams must not have expired
- PriceTracking token must not have expired
- the vault state after incrementing `borrow` to principal must be above the minimumCollateralRatio

inputs: 
fee/collateral UTXO (from USER)
VaultParams token (from Vault Validator Script)
VaultState token (from vault Validator Script)
PriceTracking Oracle Token (from oracle Validator Script)
CanMintOrSpend (from Vault Validator Script)

outputs:
fee/collateral UTXO remainder -> user Wallet
VaultParams token -> Vault Validator script
VaultState token -> Vault Validator script (increment borrowPrinciple by AddBorrowAct.amount)
PriceTracking Oracle Token -> Oracle Script
CanMintOrSpend -> Vault Validator Script

### RepayBorrowAct
- transaction must be signed by the user
- VaultParams must not have expired
- PriceTracking token must not have expired
- the user may include 1 or more dUSD Payment utxos
- dUSD payment total must cover interest applied minimum
- payment is issued against `stabilityFeeAccrued` first, then `borrowPrincipal`

inputs: 
fee/collateral UTXO (from USER)
VaultParams token (from Vault Validator Script)
VaultState token (from vault Validator Script)
PriceTracking Oracle Token (from oracle Validator Script)
CanMintOrSpend (from Vault Validator Script)
dUSD payment UTXOs (from USER)

outputs:
fee/collateral UTXO remainder -> user Wallet
VaultParams token -> Vault Validator script
VaultState token -> Vault Validator script (decrement borrowPrinciple, interestAccrued by AddBorrowAct.amount)
PriceTracking Oracle Token -> Oracle Script
CanMintOrSpend -> Vault Validator Script
Optional dUSD against principal (BURN)
dUSD against accruedInterest -> governanceScriptAddress (portion) -- provideRewards
dUSD against accruedInterest -> owner address (portion)

### UpdateStabilityFeeAct
- VaultParams must not have expired
- PriceTracking token must not have expired
- stability fee calculation is verified on chain

inputs: 
fee/collateral UTXO (from USER)
VaultParams token (from Vault Validator Script)
VaultState token (from vault Validator Script)
PriceTracking Oracle Token (from oracle Validator Script)

outputs:
fee/collateral UTXO remainder -> user Wallet
VaultParams token -> Vault Validator script
VaultState token -> Vault Validator script (increment stabilityFeeAccrued by stabilityFee)
PriceTracking Oracle Token -> Oracle Script

### UpdateVaultParamsAct
- does this create a bottleneck on Admin for the VaultParams token? same problem as oracle

inputs: 
fee/collateral UTXO (from USER)
VaultParams token (from Vault Validator Script, expired)
VaultParams token (from Admin Validator Script)

outputs:
fee/collateral UTXO remainder -> user Wallet
VaultParams token -> Vault Validator script (updated to match Admin copy)
VaultParams token -> Admin Validator Script

## Oracle Monetary Policy

tokens minted
  PriceTracking Oracle Token
  carries datum:
  { fiatPriceFeed :: AssocMap ByteString Integer -- fiat currency id to price in lovelace
  , cryptoPriceFeed :: AssocMap AssetClass Integer
  , lastUpdate :: PosixTime
  }

## Oracle Validator
parameters:
{ currencySymbol :: CurrencySymbol
, operator :: PubKeyHash
, peggedCurrency :: ByteString -- Fiat currency identifier
}

### UseOracleAct
- Oracle data must be returned to the oracle without being changed.

inputs: 
fee/collateral UTXO (from USER)
PriceTracking Oracle Token (from oracle Validator Script)

outputs:
fee/collateral UTXO remainder -> user Wallet
PriceTracking Oracle Token -> Oracle Script

### UpdateOracleAct
- tx must be signed by `operator` 

inputs: 
fee/collateral UTXO (from USER)
PriceTracking Oracle Token (from oracle Validator Script)

outputs:
fee/collateral UTXO remainder -> user Wallet
PriceTracking Oracle Token -> Oracle Script

## Buffer Monetary Policy

tokens minted: 
- LiquidationState token (1x per supported collateral currency)
  carried datum:
  { currentDiscount :: Ratio, outstandingAfterLastSale :: Integer }
- DebtSurplus State token
  { unbackedStable :: Integer
  , totalCollateralInLovelace :: Integer
  }

--TODO
Problem: how do we track the un-backed dUSD?

## Buffer Validator
parameters: { oracleAddress :: Address }
            , peggedCurrency :: ByteString -- Fiat currency identifier
            }

### LiquidationPurchaseAct

off chain code should search for vaults that can be liquidated

problem: how do we effectively seize collateral upfront, rather than liquidating in one step? can someone exploit this system to prevent being liquidated? in a price-collapse event, how can we make sure we always have liquidators?

oracle token must always show that the vault state puts the vault below minimum collateral ratio
fail if oracle token utxo is not from Oracle address or is expired
payment is applied against `stabilityFeeAccrued` first before `borrowPrinciple`
the user may provide one or more liquidation payment utxos containing dUSD
- prevent total liquidation if a partial liquidation is viable

inputs:
fee/collateral UTXO (from USER)
PriceTracking Oracle Token (from oracle Validator Script)
vault state token (from Vault for user / collateral currency)
collateral utxo (from Vault for user / collateral currency)
liquidation payment dUSD utxo (from USER)

outputs:
fee/collateral UTXO remainder -> user Wallet
PriceTracking Oracle Token -> Oracle Script
vault state token -> Vault Validator script (decrement collateral, stabilityFeeAccrued, and borrowPrinciple based on liquidation payment total)
Optional dUSD against principal (BURN)
dUSD against accruedInterest -> governanceScriptAddress (portion) -- provideRewards
dUSD against accruedInterest -> owner address (portion)
Optional collateral utxo -> Vault Validator Script (if the liquidation is not total
liquidated collateral utxo -> User wallet

### DebtPurchaseAct

-- TODO
### SurplusPurchaseAct
-- TODO

## dUSD Monetary Policy
Parameters: { initialOwner :: Address, peggedCurrency :: ByteString }

note: though `dUSD` is used below,  this is in fact a parameter-dependant token name based on `"d" <> Parameters.peggedCurrency`, by adjusting this and corresponding parameters throughout the protocol, we can easily create dGBP etc.

mintedTokens:
- UpgradeContract token
  carries Datum:
   { newContract :: Maybe Address }
- dUSD
- CanMintOrSpend token (1x for Admin Validator, 1x for Each Vault, 1x for Buffer, per upgrade)
- CostCenterState token (1x for each CostCenter)
  carries datum:
  { costCenter :: ByteString, balance :: Plutus.Value }

the dUSD has access to the `owner` address used by Admin and Governance contracts


### dUSD Token Minting
- must contain a `CanMintOrSpend` token from a script (scriptRequestingMint)

inputs:
fee/collateral UTXO (from USER)
CanMintOrSpend token (from scriptRequestingMint)

outputs:
fee/collateral UTXO remainder -> user Wallet
CanMintOrSpend token -> scriptRequestingMint
dUSD (Minted) -> (address determined by scriptRequestingMint)

note: currently there is not a way to enforce that only CanMintOrSpend minted _for_ the most recent contracts can be used to mint dUSD, without relying on the UpgradeContract token held by the Treasury Validator Script, therefore it is imperative that the Admin contract disable all child-contracts (vault, buffer) permanently once the upgrade has occured, otherwise we open ourselves up to minting for different versions of the protocol, which should be avoided.

### dUSD Token Burning
- can always succeed

inputs:
fee/collateral UTXO (from USER)
dUSD utxo (to be burned)

outputs: 
fee/collateral UTXO remainder -> user Wallet
dUSD utxo (BURN)

### CanMintOrSpend Token Minting
- the UpgradeContract token from Treasury Validator Script will have a `newContract` value with an address attached to it
- the transaction must contain a utxo from the address matching `newContract`

inputs:
fee/collateral UTXO (from USER)
UpgradeContract token (from Treasury Validator Script)
arbitrary utxo from newContract

output:
fee/collateral UTXO remainder -> user Wallet
UpgradeContract token -> Treasury Validator Script (unchanged)
arbitrary utxo from newContract -> newContract Address
CanMintOrSpend token -> (address determined by newContract Validator)

this token cannot be burned.

### UpgradeContract Token Minting
- must be signed by the `owner`
- datum always Initially set to Nothing

this will be called directly, and should only be called exactly once in the lifetime of the script, to mint 2 UpgradeContract tokens in 2 seperate utxo's

once this is completed, any attempt to mint this token should fail

inputs:
fee/collateral UTXO (from USER)

outputs:
UpgradeContract token -> Treasury Validator (set newContract to `Just (Admin Contract Address)`)
UpgradeContract token -> (newContract Admin Contract Address) (set newContract to `Nothing`)

this token cannot be burned.

## Treasury Validator
datum: ()
redeemer 
= InitiateUpgradeAct { newContract :: Address }
| SpendFromCostCenterAct { value :: Plutus.Value, costCenter :: ByteString, beneficiary :: Address }
| DepositFundsWithCostCenterAct { value :: Plutus.Value, costCenter :: ByteString }
### InitiateUpgradeAct

- the transaction must include an UpgradeContract token from the Treasury Validator script, this must specify a `newContract` which must be a `Just Address` this address will be the currentContract`
- the transaction must include an UpgradeContract token from 

inputs:
fee/collateral UTXO (from USER)
VaultParams State token (from Admin Validator Script)
UpgradeContract token (from Admin Validator Script)

outputs:
fee/collateral UTXO remainder -> user Wallet
VaultParams State token -> Admin Validator Script (updated by UpdateVaultParamsAct fields and lastUpdated = currentTime, disabledAssets = all supported asset types)
UpgradeContract token -> Treasury Validator script (set scriptAddress to InitiateUpgradeAct.newContract

### SpendFromCostCenterAct
- if no token is supplied with `costCenter` matching redeemer values, fail


inputs:
fee/collateral UTXO (from USER)
CostCenterState token (from Treasury Validator Script)
payment UTXO (from Treasury Validator Script)

outputs:
fee/collateral UTXO remainder -> user Wallet
CostCenterState token -> Treasury Validator Script, decrement`value` based on redeemer
payment utxo -> SpendFromCostCenterAct.beneficiary address

### DepositFundsWithCostCenterAct
- if a UTXO exists in the Treasury Validator for the given `costCenter`, then it must be included, otherwise one is minted
- user may include one or more utxo's as payment

inputs:
fee/collateral UTXO (from USER)
Optional CostCenterState token (from Treasury Validator Script)
payment utxos (from USER)

outputs:
fee/collateral UTXO remainder -> user Wallet
CostCenterState token (MINTED?) -> Treasury Validator Script, set `costCenter` and `value` based on redeemer
payment utxo -> Treasury Validator Script (consolidated by `costCenter`)

