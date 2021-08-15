# Ardana-dollar Endpoint specification

TODO

- debt auction algorithm
- surplus auction algorithm
- arbitrage bot spec
- reserve funds - how do stability fees and liquidations help create a reserve?
- how will stability fees be paid out

-- [Section I: Introduction](#section-i-introduction)
  * [Distinctions from MakerDAO](#distinctions-from-makerdao)
  * [Role within Ardana dApp Suite](#role-within-ardana-dapp-suite)
  * [Architecture Summary](#architecture-summary)
- [Section II: Governance](#section-ii-governance)
  * [DanaStakePool Contract](#danastakepool-contract)
    + [Deposit](#deposit)
    + [Withdraw](#withdraw)
    + [QueryStakeBalance](#querystakebalance)
    + [ProvideReward](#providereward)
  * [RewardPool](#rewardpool)
    + [Query RewardBalance,](#query-rewardbalance)
    + [ClaimReward](#claimreward)
- [Section III: Ardana-Dollar Protocol](#section-iii-ardana-dollar-protocol)
  * [Admin Contract](#admin-contract)
    + [Init-Vault](#init-vault)
    + [UpdateVaultParams](#updatevaultparams)
    + [EmergencyDisable](#emergencydisable)
    + [InitiateUpgrade](#initiateupgrade)
    + [QueryAllVaults](#queryallvaults)
    + [QueryBackedAmount](#querybackedamount)
  * [Vault Contract](#vault-contract)
    + [AddCollateral](#addcollateral)
    + [RemoveCollateral](#removecollateral)
    + [AddBorrow](#addborrow)
    + [RepayBorrow](#repayborrow)
  * [Oracle](#oracle)
    + [SetPrice](#setprice)
  * [Liquidation](#liquidation)
    + [LiquidationPurchase](#liquidationpurchase)
  * [Buffer](#buffer)
    + [DebtAuction](#debtauction)
    + [SurplusAuction](#surplusauction)
- [Section IV: Treasury](#section-iv-treasury)
  * [Treasury Contract](#treasury-contract)
    + [DepositFundsWithCostCenter](#depositfundswithcostcenter)
    + [SpendFromCostCenter](#spendfromcostcenter)
    + [AllowMint](#allowmint)
    + [AllowBurn](#allowburn)
    + [QueryCostCenters](#querycostcenters)
    + [InitiateUpgrade](#initiateupgrade-1)
- [Section V: Oracle Bot](#section-v-oracle-bot) Ardana Savings

## Section I: Introduction

This document describes the endpoint interfaces and properties for the ardana-dollar project.

This allows us to describe the behaviors seen in the Solidity source implementation (MakerDAI), and create a simple plutus interface to match

As endpoints are implemented, we will add additional information including graph-viz state machine diagrams, example calls and expected outputs, etc.

Currently, this provides information relating to the base prototype we want to have in place for the end of July, relating to the foundational stablecoin operations (minting/redeeming)

once these endpoints are in place and have test coverage/demo runs, we will add Liquidation features, Governance features, and an oracle that is more than simply stubbed out.

These contracts will distribute, Mint,  two main tokens:
1) dUSD - the Ardana Stablecoin
2) DANA - the Ardana Governance token (minted seperately)

it should be noted that both of these tokens run a 6 decimal place system similar to Ada/Lovelace

There is also an Ardana Decentralized Exchange (DEX) effort, which will support many other stablecoins.

### Distinctions from MakerDAO
In general Ardana-Dollar is modelled on systems in MakerDAO, However, unlike MakerDAO:
- There will be a fixed number of DANA tokens Minted for all time
- Liquidation 'auctions' will run automatically in sync with Oracle updates with prices determined algorithmicly based on previous sales
- the role of keepers will be reduced in scope, instead these actions will be invoked by the oracle itself.

### Role within Ardana dApp Suite 

dUSD represents an initial stablecoin offering, Ardana will later release similar offerings pegged to other fiat currencies and/or commodities.

As such the ardana-dollar project will structure itself in such a way that it can be parameterized on the necessary information specifying the USD as the pegged value of the coin. In this way, the sourcecode for ardana-dollar can be easily repurposed to support many currencies in the future, without needing large refactors.

The ardana-dollar project also includes a governance token staking contract which is isolated from other aspects of the ardana-dollar protocol and can be used to integrate with the Ardana DEX project.
### Architecture Summary

The ardana-dollar protocol will consist of 3 main smart contract components, each of which will have one or more schema (Contract) subcomponents:
- The Governance Contract: this is intended to integrate with other Ardana services, or any other strategic partner that may need to provide rewards or know vote weight for DANA Token holders. This allows for extensible and upgradable Governance activities by isolating rewards and vote weight from proposal logic.
- The Central Treasury: This houses tokens that 'belong to the protocol', as well as the minting policy, and is crucial to allowing the Ardana-dollar contract to have an upgrade path.  This portion of the contract will integrate with future versions of the protocol without ever being upgraded.
 note: this serves as a treasury for the stablecoin protocol only, this will not integrate with the dex (though it MAY integrate with more than one stablecoin should they be launched.
- Stablecoin Protocol: This includes all contracts relating to a _particular version_ of the stablecoin protocol, the settings are managed by the Admin Contract, individual vaults manage collateral, the Liquidation Contract handles Seized collateral, the Buffer handles debt and surplus auctions, and the oracle brings off-chain pricing data onto the chain, triggering various asynchronous actions throughout the system

Additionally, there is one off-chain component needed, the Oracle Bot, which will listen via Websocket to one or more price feeds, and control a wallet, which will sign transactions to be verified on-chain.

## Section II: Governance

### DanaStakePool Contract

This contract is used to distribute rewards to users and, later, determine their voting power.

the democratic actions themselves can and will be delegated to other contracts and systems such that the Ardana Dex may integrate with this contract.

#### Deposit

prerequisite: none/Contract is instantiated, user/wallet has specified amount of `Dana` supplied

input: { amount :: Integer }

behaviors:
reduce wallet balance by `amount`, lock this value in script.
update any internal state necessary to track the relative amount (increasing)

added August 11:
upon deposit, we should mint a token internally called xDANA

invariant behaviors
under no circumstances can the contract script-locked DANA reduce from interacting with this endpoint.

if the user supplies a negative, error out.

#### Withdraw

prerequisite: contract locks specified amount of DANA or greater, held for the address of the user/wallet calling this endpoint only

input: { amount :: Integer }

behaviors:
reduce script locked funds by amount of DANA specified, paying them to the user wallet
update any internal state necessary to track the relative amount (decreasing)

invariant behaviors:
under no circumstances can the user wallet decrease, except in ada fees paid.
under no circumstances can the script locked funds increase
if the user supplies a negative, error out

#### QueryStakeBalance
prerequisite: none/contract is instantiated

input: { address :: Pubkey }

returns: { amount :: Integer }

behaviors:
query endpoint returning the amount the specifying the amount of DANA currently held on behalf of the wallet address specified

this will be used to determine voting weights.

invariant behaviors:

if the address is malformed, error out. (should happen automatically)

#### ProvideReward
prerequisite: none, the contract must be instantiated
user must have provided a total amount of at least the amount specified in the input Value

input: PlutusTx.Value

behavior:
move the amount specified in the input Value from the user's wallet to the script locked contract.

when the next Epoch completes, this will be dispersed to stakers as rewards, at the proportions that the stakers had AT THAT POINT IN TIME.,

If there are no stakers when the epoch completes, then the funds are preserved until an epoch completes and there are stakers.,

### RewardPool

the reward pool manages pending reward funds and the amount pending for each account.,

importantly, rewards must be claimed by presenting xDANA tokens minted during the deposit stage.,

#### Query RewardBalance,

prerequisite: none/contract is instantiated,

input: (none)

returns PlutusTx.Value

behaviors
query endpoint returning a Value representing all tokens held as reward for a particular address,  may need to calculate all rewards accrued since the last WithdrawRewards call from this user.

this can only use the address of the caller, this cannot be queried for arbitrary addresses.

#### ClaimReward
prerequisite: none
for a non-zero result, the user must have staked DANA and have earned some rewards since the last withdrawal
input: none/()

behaviors:
if there are rewards pending for the user calling the endpoint in question, send them from the contract to the user.

if there are no rewards, error out.


## Section III: Ardana-Dollar Protocol

### Admin Contract

The Admin Contract defines the types of actions that administrators might take to steer the stablecoin configuration


There can only ever be a single Admin contract instance, otherwise there is a vulnerability

The Admin contract will need to keep track of a `minCollateralRatio` for each token type (this might just be a single value globally) - this may be something that DANA token holders will be able to update through a democratic process, which will need to adjust all vaults currently running as well as future vaults when changed

The Admin Contract will permit a single `adminAddress` to make adjustments to its config, this is one of the more centralized aspects of the system in v1 and will later be subsumed by democratic Proposal functions using vote weights from the Governance Contract in v2. 

The Admin Contract is parameterized on a `baseCurrency` (this may be expanded to a record), such that the first and primary contract we will build uses "USD" to  produce dUSD, but by deploying with a different parameter, we could easily use "JPY" to make dJPY etc. Each of these parameterized items would require a seperate Treasury, and by extension it's own DANA Float.

note: we may be able to extend the configuration of the treasury from a single contract address to a list, however this may require further examination and consideration.

all other structures in the ardana-Dollar protocol are parameterized on currency in this way, as is the oracle bot.

#### Init-Vault

prerequisite: none

input: 
VaultConfig :: { supportedToken :: AssetClass }

expected behaviour: 

calling this endpoint instantiates a Vault Contract for the user's address with `VaultConfig` as its config params (one vault per `supportedToken`, per user), such that the user can then activate the Vault Contract with their Wallet and immediately call `MintUSD`

#### UpdateVaultParams

prerequisite:  the user's address must match a hardcoded value

input: { configs :: [(VaultConfig, Rational)], -- asset class and the default stability fee multiplier for dUSD loans backed by that asset class
       , liquidationBenefitCap :: Rational
       , liquidationBenefitFloor :: Rational
       , minCollateralRatio :: Rational
       }

behavior: updates collateralRatio and other critical config for all existing vaults, as well any new vaults will use the supplied configs.

VaultConfig assetClasses must ALL be supported by the Oracle Price feed
(Ada is a special case since all other pricing will be converted to lovelace as a base currency for the system)

`liquidationBenefitCap` must be greater than `liquidationBenefitFloor` and neither can have a zero in the denominator position, or be negative.

#### EmergencyDisable
prerequisites: user's address must match a hardcoded value

input: [AssetClass]

all Vaults for `AssetClass` will not permit new loans, only repayments

#### InitiateUpgrade
prerequisite: user's address must match a hardcoded value

input: { newContract :: Address }

calls Treasury.InitiateUpgrade endpoint, sending a unique token to the Treasury which can be verified.

calls EmergencyDisable for all supported currencies

#### QueryAllVaults
returns all vaults for the user

#### QueryBackedAmount
returns total # of vaults, total amount of collateral, and total amount of [DUSD](DUSD) in circulation

### Vault Contract
The Vault contract a single supported collateral type whose value can be obtained through a reliable oracle. Which collateral types are supported is decided by the Admin Contract.  For now this can be stubbed out as some configuration used in the contract, which can be changed.  

Vault Config values:
supportedToken :: AssetClass - tokens permitted for collateral use.
minCollateralRatio :: Rational - default: (150%) - the minimum collateral/borrow value ratio permitted before liquidating an account's assets (ie the user can borrow 100 Ada worth of dUSD for every 120 Ada supplied as collateral)
userAddress :: Address - the user address this vault is 'for'
(other config values may be added for fee calculations, etc)

In practice, Collaterial Ratios are encouraged to be much higher than the minimum, more like 1:5 - 1:10. This insulates borrowers from price volatility and prevents liquidation events.

In order to compare collateral to the value of USD, we will require an Oracle.  eventually this will require the use of an Oracle bot

Users can lock supported assets into their vault minting dUSD at a given collateral Ratio, users can return dUSD to the contract along with a collateral ratio to be maintained after the redemption (in the case of a partial redemption). This is also called a Collateralized debt position.

There will be many vault contract instances, each with a unique `supportedToken` - all of which mint dUSD.

the `supportedToken` for a vault may also be referred to as the underlying token of a vault.

Note: Although the Vault contract must be used to Mint new dUSD, the actual minting must also be approved by the Treasury, as the Treasury directly connects to the the MonetaryPolicy which will mint dUSD.

#### AddCollateral
prerequisites: Vault for token must be instantiated, 

input { amount :: Integer }

behavior:
move `amount` of the Vault's assetClass from the user wallet to the script.

if the user has less than this amount, error out

error out if `amount` is negative

#### RemoveCollateral
prerequisites: Vault for token must be instantiated, 
user must have at least the specified amount of collateral in the vault (must have called AddCollateral)

input: { amount :: Integer }

behavior:
if removing this amount would bring the user's overall collateralRatio below the minimumCollateralRatio, OR if the user does not have `amount` of the Vault's assetclass deposited as collateral, error out.
otherwise, move `amount` of the Vault's assetClass from the script to the user.

error out if `amount` is negative

#### AddBorrow

prerequisites: Vault for token must be instantiated, 
user must have appropriate collateral locked in the vault.

input:
AddBorrow :: { amount :: Integer }
expected behavior:

if adding `amount` to the user's current borrow would  result in the user exceeding the minumumCollateralRatio, error out.

the user's wallet balance will be increased by `amount` of dUSD.

invariant inputs/behaviors:

under no circumstances (ie, negative input) can this endpoint RELEASE underlying tokens back to the user, or burn tokens of any kind.

error out if `amount` is negative

#### RepayBorrow
prerequisites:
the vault contract is instantiated
the user has deposited collateral
the user has taken out a dUSD Loan

input: { amount: Integer }

behavior:
calculate the `stabilityfee` ahead of time and add it to the outstanding balance of the borrow.
repays `amount` of dUSD against the existing borrow on a given vault.
error out if the user does not have `amount` of dUSD

error out if `amount` is negative.

it's important to track the principal of a given dUSD loan, and the portion which comes from stability fees, as a portion of the stabilityfee is paid out the the DANAStakePool as a reward, the rest will be sent to an Ardana-controlled wallet which is used to manage costs associated with the protocol.

if a loan is taken out and repaid between Oracle.SetPrice calls, then there is no stability fee

### Oracle

#### SetPrice

prerequisite:
none

input: { oraclePK :: Crypto.PubKey, oracleMsg :: Oracle.SignedMessage(Oracle.Observation a) }

where `a` is sufficient info to gain the time-weight averaged price of USD in lovelace.

updates prices used for transactions, 
applies stabilityFee to vaults - any seizures will include stabilityFee in their calculations
triggers liquidation seizures, making these funds available to callers of the LiquidationPurchase Endpoint.

liquidation seizures claim the equivalent amount of collateral necessary to bring the user back to a safe collateralRatio for the given Vault (Plus liquidationBenefitCap as a percentage) 

calculating liquidation seizures:
let loanAmountInAda = dUSDLoanTotal * DUSD:Ada price
let collateralTotalInAda = totalCollateral * Collateral:Ada price
let currentRatio = collateralTotalInAda / loanAmountInAda
let liquidationMultiplier = (1 + liquidationBenefitCap)
let nearestSafeLoanAmountInAda = (collateralTotalInAda / maximumCollateralRatio) / liquidationMultiplier
let seizedAmount = (loanAmountInAda - nearestSafeLoanAmountInAda) * liquidationMultiplier
let newCollateralAmountInAda = collateralTotalInAda - seizedAmount

when a seizure occurs, 
 the user's principle borrow is now equal to the `nearestSafeLoanAmountInAda` / DUSD:Ada Price
 the user's collateral is now `newCollateralAmountInAda` (converted back to the underlying currency)
 if this results in the user's collateral Ratio being less than `minimumCollateralRatio` we need to adjust this formula.
 the seized funds are sent to the liquidation module

this endpoint will rely on a bot which polls a centralized server, and controls a wallet to submit this information on-chain

calculating stability fees:
- each oracle call will calculate the POSIXTimeRange since the previous oracle call
- the stability fee Per vault will calculate the amount of interest against the principle to charge such that at the end of 1 year, the loan amount will be equal to the principal * (1 + `stabilityForAssetClass`) - this stabilityForAssetClass is the Ratio supplied along with VaultConfigs.
- this must always use the CURRENT stabilityFee, regardless of the one that was in place when the vault was created.

No Matter how many times `SetPrice` is called, this endpoint should only execute every 10 minutes

### Liquidation

The Liquidation algorithmicly runs an 'auction' to sell off collateral when a user falls below the minimumCollateralRatio

#### LiquidationPurchase
prerequisite: there is at least one vault where the minimumCollateralRatio is not met.

input: { amount :: Integer, assetClass :: AssetClass }

behavior:
transfer `amount` of assetClass to user (if there is that amount of that assetClass available for liquidation and assuming the price can be paid)
user must pay `amount` * currentLiquidationPrice for that collateral type in DUSD

the liquidation purchase must be able to track previous purchases of the `assetClass` specified by the user over the past several liquidations

If this is the First liquidation of this assetClass in more than 24 hours,  the price of the seized collateral is the market price (Value of the collateral in ada, converted to USD/DUSD) minus liquidationBenefitCap.
If we have a record of a previous liquidation 24 AND we sold All of the given `assetClass`, increase the previous price by 1%
If we did not sell all of the given assetClass, decrease the price by 1%
the maximum and minimum bounds for the price are marketPrice * liquidationBenefitFloor and marketPrice * liquidationBenefitCap.

### Buffer

#### DebtAuction
prerequisites: 
there is more DUSD in circulation than total collateral value in Ada.

input : { amount :: Integer }

behavior:
user purchases `amount` of DANA tokens using DUSD at `currentDebtAuctionPrice`

#### SurplusAuction
prerequisites:
there is a greater amount of Collateral than the value of all DUSD loans.

input: { amount :: Integer }

behavior:
user purchases `amount` of DUSD using DANA at `currentSurplusAuctionPrice`

## Section IV: Treasury

### Treasury Contract
The treasury holds all reserve funds, and other funds that belong 'to the protocol' and cannot be withdrawn directly by users, The treasury also manages the MonetaryPolicy used to mint dUSD such that upgrades to the ardana-dollar will not force a change in the monetaryPolicy used, as this would cause there to be 2 different 'dUSD' coins in circulation.

The treasury contract will not receive upgrades, instead each version of the various Ardana-dollar contracts which integrate with it must receive a message from a centralized source (later, the Governance/DanaStakePool contracts), which will tell it to allow interaction with other contracts. Additionally, the treasury must control the MonetaryPolicy that mints DUSD and it also delgates this.
This module must function in such a way that the protocol can receive an upgrade without needing to trade in old DUSD from a previous protocol for new DUSD, or risking important funds being released

 the treasury must track `currentContract` which is the contract that it delegates spending and DUSD minting operations to.
 
 the treasury holds rewards and DANA tokens which are not yet issued to users, it also holds an amount of DUSD and DANA which are to be used exclusively for auctions.

Similar To the LiquidatonModule, the Treasury holds auctions where prices are determined algorithmicly


#### DepositFundsWithCostCenter
prerequisites: the user must have provided `amount` of `currency`

input :: { amount :: Integer, currency :: AssetClass, costCenter:: ByteString }

behavior: 
move `amount` of `currency` from user wallet to the script address, such that the sum `Value` associated to each `costCenter` can be calculated

this allows for an extensible set of funds in the treasury, which can be spent by the `currentContract`, this can be used as a rudimentary accounting system which prevents the system from overspending in edge-case scenarios.

examples `{ amount = 1000000000, currency = danaAssetClass, costCenter = "SurplusAuctionFloat" }`
examples `{ amount = 1000000000, currency = Ada, costCenter = "ProtocolReserve" }`

no negative inputs permitted

#### SpendFromCostCenter
prerequisite: the transaction must witness proof of the Admin contract's code (specifically a minted token that the admin contract has exclusive rights to mint).
`value` must be previously deposited to the indicated costCenter

input :: { value :: Plutus.Value, costCenter :: ByteString, beneficiary :: Address }

behavior:
pay `value` from `costCenter` to `beneficiary`, if the funds are available

no negative inputs permitted

#### AllowMint
prerequisite: the transaction must witness proof of the Admin contract's code (specifically a minted token that the admin contract has exclusive rights to mint).

input :: [(Integer, Address)]

behavior:
allows the minting of dUSD (or the relavant ardana-dollar protocol stablecoin)

Mints the total of all the Integers in the first Tuple position
sends each amount specified in the first position, to the corresponding address in the second position.

no negative inputs permitted

#### AllowBurn
prerequisite: the transaction must witness proof of the Admin contract's code (specifically a minted token that the admin contract has exclusive rights to mint).
the caller must have `amount` of dUSD provided to the transaction

input :: { amount :: Integer }

behavior:
Allows the burning of dUSD (or the relavant ardana-dollar protocol stablecoin)

the caller must provide the `amount` in dUSD, it is burned.

no negative inputs permitted

#### QueryCostCenters
prerequisite: none

input: none /()

returns [(ByteString, Value)]

where the first position represents each cost center and the second position represents the Value stored for that cost center.

#### InitiateUpgrade
prerequisites:
the treasury must receive a unique token which can only be supplied by the `currentContract`, this will hold the next `currentContract` in it's datum
the transaction must be signed by a hardcoded userAddress.

input: { newContract :: Address }

behaviors:
if the transaction meets security prerequisites, then the newContract is accepted as the `currentContract`, the previous contract must be put into emergencyDisabled mode

after this process, the old contract can pay funds into the treasury, but not extract funds, receive reward DANA, or mint new DUSD

## Section V: Oracle Bot

this will be a small haskell program that integrates with an off-chain price feed from a centralized fiat currency exchange, ideally by websocket, alternatively by http polling.

upon price updates it will cryptographically verify the price feed signature, then sign a call to 'SetPrice', reformatting it to the plutus `Data` encoding

this bot will control a wallet, which will need to keep Ada on hand for transaction fees in order to guarantee regular oracle pricing updates.



