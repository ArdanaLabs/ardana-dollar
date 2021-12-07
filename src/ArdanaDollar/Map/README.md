# Distributed map implementation based on on-chain sorted linked list

Based on [the design](https://mlabs.slab.com/posts/on-chain-association-list-with-constant-time-insert-removal-sh8z2xzy)

## Performance characteristics

Insertion and deletion is O(1) in time and in space.
Modyfying existing entries is O(1) in time and in space.
Snapshot making is O(log(n)) in time and O(n) in space.

When snapshot is in progress a map cannot be modified.

## Scripts sizes

As for now the main validator is ~10kB and minting policies ~12kB.

Each transaction requires the main validator and one of the minting policies to be included, thus ~22kB

## Testing

There are a few sunny day scenario tests implemented with EmulatorTrace. The implementation hasn't been tested with real blockchain and most probably it won't work.

## Missing features / bugs

Snapshot making for a map consisting of $N$ entries requires $N$ transactions to be executed in parallel. The first issue is that in real blockchain it's not possible to execute arbitrarily number of transactions in one block. The other issue is that the user submitting those transaction has provide $N$ own utxos for fees. In other words the user has prepare for snapshot making by ensuring that there are sufficient utxos to power parallel transactions.

Off-chain code cannot resume snapshot that has been interrupted. On-chain accounts for this.

The distributed map should be parametrizable by an application token. Map validator should check that in each transaction application token is minted or burnt. That guarantees that its minting policy is fired. This mechanism provides validation logic delegation.

## Improvements

Folowing operations can be optimized by consuming/producing more utxos (>2) in one transaction:
- snapshot permission splitting - split snapshot permission into many snapshot permissions in one transaction  
- snapshot making - make snapshot of many map entries in one transaction
- unlock permision merging - merge many unlock permissions in one transaction

Split minting policies into smaller minting policies. E.g. *NodeValidPolicy* validates many insertion/deletion scenarios. Is it possible to have more fine-grained validation?

Having [Proposal](https://github.com/cardano-foundation/CIPs/pull/159) gives a new implementation opportunity for a distributed map. Such implementation would be O(1) for insertion, deletion, modification and even for snapshot making! The proposal is a game changer for governance.




