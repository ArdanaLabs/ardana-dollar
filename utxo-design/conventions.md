# Conventions for the specification

(incomplete)

## Optional datums

See section 3.1 of https://hydra.iohk.io/build/7940505/download/1/alonzo-changes.pdf

Specifically, the corresponding datums for some datum hashes
are not included in the transaction body, meaning that validation
can possibly fail in such a case.

Concretely, this means that it will be missing from `txInfoData`.

This is often relevant for state machines that inspect the
datum of the continuing output.
**However**, it is **not** required, if rather than inspecting it,
you check whether the hash matches the hash of another datum.

When such an "optional datum" is required for a validator to
function, it will be explicitly noted.

Relevant: https://github.com/input-output-hk/cardano-node/issues/2919

## Fees

Handling of fees is implicit, as it's often not important.

## Parameters

Each part of the specification can be parameterized arbitrarily,
meaning that we have a family of it.

## State machine

State machines are a pattern, whereby some state is
locked in a datum by a validator, that only allows using
the UTXO if the redeemer represents a valid transitions,
which we will call an *act*.
These acts are essentially actions that can be taken.

The consuming transaction will in general be required to
preserve the state machine by containing a UTXO with the
updated datum and same validator.
This is assumed to be the default.

State machines will have *state tokens* that certify
the correctness of a state machine instance, unless otherwise noted.
Specifically, the minting policy for such a token will only allow minting if a
state machine instance with the correct initial state is made.
Otherwise, you could construct the instance with arbitrary state.

It is important to note that it should be possible to construct
multiple instances in the same transaction. Concretely, this means that
the minting policy should check that each UTXO with the token satisfies the
constraints separately, taking the context into account.

The token can also be non-fungible, meaning that only one
instance can ever exist.

### Specification

Datum: State contained.
Acts (multiple): Possible state transitions. Validation rules will be specified.
Initial: The permissible initial state and the context necessary (e.g. required UTXOs).

## State machine certification token

```haskell
certTokenMP :: MintingPolicy
```

### Motivation

For a given state machine, we will often need to access
its state in other scripts. The problem is that, given that it's a state
machine, reading it naively would create immense contention, as it
can only ever be used once per block.

We solve this problem by not reading the state directly
in a script, but by requiring that the consumer supply it
in the redeemer. The state then needs to be checked for validity,
and that is done by having a minting policy, the *certification tokens* of which
contain the hash of the state and the timestamp in the name of the token itself.

Concretely: The "client" validators will fail if the hash of the {state + some extra
stuff} supplied in the redeemer is not equal to the name of the token
of the relevant minting policy.

### Minting policy

```haskell
type CertTokenMPRedeemer = [AssetClass]
```

Each token of this policy minted must correspond to a consumed
input in the following way:
```haskell
data CertifiedDatum = CertifiedDatum
  { timestamp :: POSIXTime
  , token :: AssetClass
  , unCertifiedDatum :: Datum
  }

-- **Must** use sha2_256
certHash :: CertifiedDatum a -> BuiltinByteString

correpondsToInput :: TokenName -> AssetClass -> TxInfo -> TxOut -> Bool
correpondsToInput (TokenName name) token info input =
  uncurry (valueOf . txOutValue input) (unAssetClass token) > 0
  && certHash certDatum == name
  where
    certDatum =
      CertifiedDatum
        { timestamp = ivFrom $ txInfoValidRange info
        , token = token
        , unCertifiedDatum = findDatum (txOutDatumHash input) info
        }
```

The `AssetClass`es we test against come from the redeemer
for the minting policy script.

- If an `x` and `y` can be found such that `correpondsToInput token x y ≡ true`,
  then minting is allowed.
- Minting is also allowed if we already have an instance of the token,
  i.e. you can duplicate it.
- Burning is always allowed.
- Otherwise it's not allowed.

### Using it downstream

A downstream validator will take the certification token's name
in the redeemer, check that such a token exists in the transaction,
then fetch the corresponding data from `txInfoData` by looking up
the datum using the token name, that is essentially the hash of the datum.

**NB:** This means that the submitter of the transaction **must**
remember to include the datum as a witness in the transaction.

### Ensuring proliferation

The validator described here is parameterized with the state machine
in question.

In the common case, the state machine will on each transition,
i.e. in each transaction that consumes the state machine,
there will be `X` outputs with the certification token.
These outputs will be locked to a validator, that
requires you to produce Y outputs with the same token
if you consume it.

If all available UTXOs are consumed in each block,
there will be `Y^n * X` available UTXOs after `n` blocks.

`Y` and `X` can be hard-coded parameters, or depend on the state itself.

There will be another parameter `Z` for controlling
how much time can pass before a certain token has expired.
This parameter can also be a part of the state itself.

Specifically, it will be the maximum difference between
the maximum allowed timestamp for the consuming transaction
and the minimum allowed timestamp for the transaction that produced
the certification token.

#### Clean-up

Each UTXO needs a minimum amount of Ada, which would be a substantial extra
fee if users make UTXOs that can never be cleaned-up.
To prevent this, each UTXO with the validator for certification tokens
will have an `Address` in the datum. The consuming transaction
**must** have an output that sends the Ada in the UTXO to this `Address`.

In addition, if **and only if** the consuming input has a redeemer
which can be decoded as the state the token represents, then there
are no restrictions on how many continuing UTXOs there must be
**if the token has expired**.

This means that after the token has expired, you can retrieve the Ada
locked by the validator without creating more UTXOs with more locked Ada.

#### Off-chain usage

In the off-chain code, when we need to supply a token certifying
the state, we will find the latest transaction consuming the state machine
in the ledger, then attempt to use one of its `X` outputs containing
the certification token.
If this fails because of contention, we will find the transaction that used
the output we tried to use, and use one of its `Y` outputs.
If this fails again, we will recurse again, and so on, until we find a UTXO.

Due to the exponential nature, it is very hard for an attacker
to consume all available UTXOs, since the exponential growth will
mean an exponentially increasing amount of effort is needed for each
block.

### Specification

A state machine will have a "certification token" section.
