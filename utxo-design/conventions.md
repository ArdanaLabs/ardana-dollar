# Conventions for the specification

(incomplete)

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

State machines will often have *state tokens* that certify
the correctness of a state machine instance.
Specifically, the minting policy for such a token
will only allow minting if a state machine instance
with the correct initial state is made.
Otherwise, you could construct the instance with arbitrary state.

The token can also be non-fungible, meaning that only one
instance can ever exist.

### Specification

Datum: State contained.
Acts (multiple): Possible state transitions. Validation rules will be specified.
Token: The token used to represent the state machine.

## State machine certification token

For a given state machine, we will often need to access
its state in other scripts. The problem is that, given that it's a state
machine, reading it naively would create immense contention, as it
can only ever be used once per block.

We solve this problem by not reading the state directly
in a script, but by requiring that the consumer supply it
in the redeemer. The state then needs to be checked for validity,
and that is done by having a minting policy, the *certification tokens* of which
contain the hash of the state and the timestamp in the name of the token itself.

Concretely: The validator will fail if the hash of the state and timestamp
supplied in the redeemer is not equal to the name of the token
of the relevant minting policy.

The minting policy will be that:
- You can mint if you have one already.
- You can always burn.
- You can also mint if you're consuming a UTXO from the state machine
  in question, and the token name is equal to the hash of the datum
  and timestamp.

In the common case, the state machine will on each transition,
i.e. in each transaction that consumes the state machine,
there will be `X` outputs with the certification token.
These outputs will be locked to a validator, that
requires you to produce Y outputs with the same token
if you consume it.

If all available UTXOs are consumed in each block,
there will be `Y^n * X` available UTXOs after `n` blocks.

`Y` and `X` can be hard-coded parameters, or depend on the state itself.

The reason that we include a timestamp in the hashed data, is so that
validators can check how old the token is.
In the common case, there will be another parameter `Z` for controlling
how many slots can pass before a certain token has expired. This parameter
can also be a part of the state itself.

### Off-chain usage

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
