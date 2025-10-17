(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-BOUNTY (err u101))
(define-constant ERR-BOUNTY-NOT-FOUND (err u102))
(define-constant ERR-INSUFFICIENT-FUNDS (err u103))
(define-constant ERR-PROOF-REQUIRED (err u104))
(define-constant ERR-ALREADY-CLAIMED (err u105))
(define-constant ERR-ALREADY-VOTED (err u106))
(define-constant ERR-INSUFFICIENT-VERIFICATIONS (err u107))
(define-constant ERR-SPECIES-NOT-FOUND (err u108))
(define-constant ERR-INSUFFICIENT-MATCHING-FUNDS (err u109))
(define-constant ERR-NO-MATCHING-PLEDGE (err u110))

(define-data-var contract-owner principal tx-sender)
(define-data-var total-trees-planted uint u0)
(define-data-var bounty-counter uint u0)
(define-data-var min-verifications uint u3)
(define-data-var base-reputation uint u100)
(define-data-var max-reputation uint u1000)
(define-data-var total-co2-absorbed uint u0)
(define-data-var total-oxygen-produced uint u0)
(define-data-var total-soil-improved uint u0)

(define-map bounties
    uint
    {
        creator: principal,
        reward-per-tree: uint,
        total-reward: uint,
        remaining-reward: uint,
        trees-required: uint,
        trees-planted: uint,
        expiry: uint,
        active: bool,
    }
)

(define-map tree-proofs
    {
        bounty-id: uint,
        planter: principal,
    }
    {
        gps-lat: (string-utf8 50),
        gps-long: (string-utf8 50),
        photo-hash: (string-utf8 64),
        verified: bool,
        claimed: bool,
        verification-count: uint,
        species: (string-utf8 32),
        region: (string-utf8 32),
    }
)

(define-map community-verifications
    {
        bounty-id: uint,
        planter: principal,
        verifier: principal,
    }
    {
        vote: bool,
        block-height: uint,
    }
)

(define-map planter-stats
    principal
    {
        total-trees: uint,
        total-rewards: uint,
    }
)

(define-map user-reputation
    principal
    {
        score: uint,
        correct-verifications: uint,
        total-verifications: uint,
        last-updated: uint,
    }
)

(define-map tree-species
    (string-utf8 32)
    {
        co2-per-year: uint,
        oxygen-per-year: uint,
        soil-improvement: uint,
    }
)

(define-map regional-impact
    (string-utf8 32)
    {
        trees-count: uint,
        co2-absorbed: uint,
        oxygen-produced: uint,
        soil-improved: uint,
    }
)

(define-map sponsor-matches
    {
        bounty-id: uint,
        sponsor: principal,
    }
    {
        pledged-amount: uint,
        remaining-amount: uint,
        match-ratio: uint,
    }
)

(define-map bounty-total-matches
    uint
    {
        total-pledged: uint,
        total-remaining: uint,
    }
)

(define-read-only (get-bounty (bounty-id uint))
    (ok (unwrap! (map-get? bounties bounty-id) ERR-BOUNTY-NOT-FOUND))
)

(define-read-only (get-tree-proof
        (bounty-id uint)
        (planter principal)
    )
    (map-get? tree-proofs {
        bounty-id: bounty-id,
        planter: planter,
    })
)

(define-read-only (get-planter-stats (planter principal))
    (default-to {
        total-trees: u0,
        total-rewards: u0,
    }
        (map-get? planter-stats planter)
    )
)

(define-read-only (get-user-reputation (user principal))
    (default-to {
        score: (var-get base-reputation),
        correct-verifications: u0,
        total-verifications: u0,
        last-updated: u0,
    }
        (map-get? user-reputation user)
    )
)

(define-read-only (calculate-reputation-weight (user principal))
    (let ((reputation (get-user-reputation user)))
        (/ (get score reputation) (var-get base-reputation))
    )
)

(define-read-only (get-community-verification
        (bounty-id uint)
        (planter principal)
        (verifier principal)
    )
    (map-get? community-verifications {
        bounty-id: bounty-id,
        planter: planter,
        verifier: verifier,
    })
)

(define-read-only (get-tree-species (species (string-utf8 32)))
    (map-get? tree-species species)
)

(define-read-only (get-regional-impact (region (string-utf8 32)))
    (default-to {
        trees-count: u0,
        co2-absorbed: u0,
        oxygen-produced: u0,
        soil-improved: u0,
    }
        (map-get? regional-impact region)
    )
)

(define-read-only (get-total-environmental-impact)
    {
        total-trees: (var-get total-trees-planted),
        total-co2-absorbed: (var-get total-co2-absorbed),
        total-oxygen-produced: (var-get total-oxygen-produced),
        total-soil-improved: (var-get total-soil-improved),
    }
)

(define-read-only (get-sponsor-match
        (bounty-id uint)
        (sponsor principal)
    )
    (map-get? sponsor-matches {
        bounty-id: bounty-id,
        sponsor: sponsor,
    })
)

(define-read-only (get-bounty-matching-pool (bounty-id uint))
    (default-to {
        total-pledged: u0,
        total-remaining: u0,
    }
        (map-get? bounty-total-matches bounty-id)
    )
)

(define-public (create-bounty
        (reward-per-tree uint)
        (trees-required uint)
        (expiry uint)
    )
    (let (
            (bounty-id (+ (var-get bounty-counter) u1))
            (total-reward (* reward-per-tree trees-required))
        )
        (try! (stx-transfer? total-reward tx-sender (as-contract tx-sender)))
        (map-set bounties bounty-id {
            creator: tx-sender,
            reward-per-tree: reward-per-tree,
            total-reward: total-reward,
            remaining-reward: total-reward,
            trees-required: trees-required,
            trees-planted: u0,
            expiry: expiry,
            active: true,
        })
        (var-set bounty-counter bounty-id)
        (ok bounty-id)
    )
)

(define-public (submit-tree-proof
        (bounty-id uint)
        (gps-lat (string-utf8 50))
        (gps-long (string-utf8 50))
        (photo-hash (string-utf8 64))
        (species (string-utf8 32))
        (region (string-utf8 32))
    )
    (let (
            (bounty (unwrap! (map-get? bounties bounty-id) (err ERR-BOUNTY-NOT-FOUND)))
            (current-height burn-block-height)
        )
        (asserts! (< current-height (get expiry bounty)) (err ERR-INVALID-BOUNTY))
        (asserts! (get active bounty) (err ERR-INVALID-BOUNTY))
        (asserts! (is-some (map-get? tree-species species))
            (err ERR-SPECIES-NOT-FOUND)
        )
        (map-set tree-proofs {
            bounty-id: bounty-id,
            planter: tx-sender,
        } {
            gps-lat: gps-lat,
            gps-long: gps-long,
            photo-hash: photo-hash,
            verified: false,
            claimed: false,
            verification-count: u0,
            species: species,
            region: region,
        })
        (ok true)
    )
)

(define-public (community-verify-tree
        (bounty-id uint)
        (planter principal)
        (vote bool)
    )
    (let (
            (proof (unwrap!
                (map-get? tree-proofs {
                    bounty-id: bounty-id,
                    planter: planter,
                })
                (err ERR-PROOF-REQUIRED)
            ))
            (existing-vote (map-get? community-verifications {
                bounty-id: bounty-id,
                planter: planter,
                verifier: tx-sender,
            }))
            (current-count (get verification-count proof))
            (verifier-weight (calculate-reputation-weight tx-sender))
            (weighted-vote (if vote
                verifier-weight
                u0
            ))
            (new-count (+ current-count weighted-vote))
        )
        (asserts! (is-none existing-vote) (err ERR-ALREADY-VOTED))
        (asserts! (not (is-eq tx-sender planter)) (err ERR-NOT-AUTHORIZED))
        (map-set community-verifications {
            bounty-id: bounty-id,
            planter: planter,
            verifier: tx-sender,
        } {
            vote: vote,
            block-height: burn-block-height,
        })
        (begin
            (unwrap-panic (update-verifier-reputation tx-sender vote))
            (if vote
                (begin
                    (map-set tree-proofs {
                        bounty-id: bounty-id,
                        planter: planter,
                    }
                        (merge proof { verification-count: new-count })
                    )
                    (if (>= new-count
                            (* (var-get min-verifications)
                                (var-get base-reputation)
                            ))
                        (begin
                            (map-set tree-proofs {
                                bounty-id: bounty-id,
                                planter: planter,
                            }
                                (merge proof {
                                    verification-count: new-count,
                                    verified: true,
                                })
                            )
                            (let ((bounty (unwrap! (map-get? bounties bounty-id)
                                    (err ERR-BOUNTY-NOT-FOUND)
                                )))
                                (begin
                                    (unwrap-panic (update-environmental-impact
                                        (get species proof)
                                        (get region proof)
                                    ))
                                    (map-set bounties bounty-id
                                        (merge bounty { trees-planted: (+ (get trees-planted bounty) u1) })
                                    )
                                    (var-set total-trees-planted
                                        (+ (var-get total-trees-planted) u1)
                                    )
                                    (ok true)
                                )
                            )
                        )
                        (ok true)
                    )
                )
                (ok true)
            )
        )
    )
)

(define-private (update-verifier-reputation
        (verifier principal)
        (correct-vote bool)
    )
    (let (
            (current-rep (get-user-reputation verifier))
            (new-total-verifications (+ (get total-verifications current-rep) u1))
            (new-correct-verifications (if correct-vote
                (+ (get correct-verifications current-rep) u1)
                (get correct-verifications current-rep)
            ))
            (accuracy-rate (if (> new-total-verifications u0)
                (/ (* new-correct-verifications u100) new-total-verifications)
                u50
            ))
            (reputation-adjustment (if (> accuracy-rate u75)
                u10
                (if (< accuracy-rate u25)
                    u20
                    u0
                )
            ))
            (new-score (if (> accuracy-rate u75)
                (let ((proposed-score (+ (get score current-rep) reputation-adjustment)))
                    (if (> proposed-score (var-get max-reputation))
                        (var-get max-reputation)
                        proposed-score
                    )
                )
                (if (< accuracy-rate u25)
                    (let ((proposed-score (- (get score current-rep) reputation-adjustment)))
                        (if (< proposed-score u10)
                            u10
                            proposed-score
                        )
                    )
                    (get score current-rep)
                )
            ))
        )
        (map-set user-reputation verifier {
            score: new-score,
            correct-verifications: new-correct-verifications,
            total-verifications: new-total-verifications,
            last-updated: burn-block-height,
        })
        (ok true)
    )
)

(define-public (verify-tree-proof
        (bounty-id uint)
        (planter principal)
    )
    (let (
            (proof (unwrap!
                (map-get? tree-proofs {
                    bounty-id: bounty-id,
                    planter: planter,
                })
                (err ERR-PROOF-REQUIRED)
            ))
            (bounty (unwrap! (map-get? bounties bounty-id) (err ERR-BOUNTY-NOT-FOUND)))
        )
        (asserts! (is-eq tx-sender (var-get contract-owner))
            (err ERR-NOT-AUTHORIZED)
        )
        (map-set tree-proofs {
            bounty-id: bounty-id,
            planter: planter,
        }
            (merge proof {
                verified: true,
                verification-count: u1,
            })
        )
        (begin
            (unwrap-panic (update-environmental-impact (get species proof) (get region proof)))
            (map-set bounties bounty-id
                (merge bounty { trees-planted: (+ (get trees-planted bounty) u1) })
            )
            (var-set total-trees-planted (+ (var-get total-trees-planted) u1))
            (ok true)
        )
    )
)
(define-public (claim-reward (bounty-id uint))
    (let (
            (proof (unwrap!
                (map-get? tree-proofs {
                    bounty-id: bounty-id,
                    planter: tx-sender,
                })
                (err ERR-PROOF-REQUIRED)
            ))
            (bounty (unwrap! (map-get? bounties bounty-id) (err ERR-BOUNTY-NOT-FOUND)))
            (stats (default-to {
                total-trees: u0,
                total-rewards: u0,
            }
                (map-get? planter-stats tx-sender)
            ))
            (reward (get reward-per-tree bounty))
            (matching-pool (get-bounty-matching-pool bounty-id))
            (available-match (get total-remaining matching-pool))
            (match-amount (if (> available-match u0)
                reward
                u0
            ))
            (total-payout (+ reward match-amount))
        )
        (asserts! (get verified proof) (err ERR-PROOF-REQUIRED))
        (asserts! (not (get claimed proof)) (err ERR-ALREADY-CLAIMED))
        (asserts! (<= reward (get remaining-reward bounty))
            (err ERR-INSUFFICIENT-FUNDS)
        )
        (let ((base-transfer (stx-transfer? reward (as-contract tx-sender) tx-sender)))
            (match base-transfer
                success (begin
                    (if (> match-amount u0)
                        (begin
                            (unwrap-panic (as-contract (stx-transfer? match-amount tx-sender tx-sender)))
                            (unwrap-panic (distribute-matching-funds bounty-id match-amount))
                            true
                        )
                        true
                    )
                    (map-set tree-proofs {
                        bounty-id: bounty-id,
                        planter: tx-sender,
                    }
                        (merge proof { claimed: true })
                    )
                    (map-set bounties bounty-id
                        (merge bounty { remaining-reward: (- (get remaining-reward bounty) reward) })
                    )
                    (map-set planter-stats tx-sender {
                        total-trees: (+ (get total-trees stats) u1),
                        total-rewards: (+ (get total-rewards stats) total-payout),
                    })
                    (ok true)
                )
                error (err ERR-INSUFFICIENT-FUNDS)
            )
        )
    )
)
(define-public (close-bounty (bounty-id uint))
    (let (
            (bounty (unwrap! (map-get? bounties bounty-id) (err ERR-BOUNTY-NOT-FOUND)))
            (remaining (get remaining-reward bounty))
        )
        (asserts! (is-eq tx-sender (get creator bounty)) (err ERR-NOT-AUTHORIZED))
        (match (stx-transfer? remaining (as-contract tx-sender) (get creator bounty))
            success (begin
                (map-set bounties bounty-id
                    (merge bounty {
                        active: false,
                        remaining-reward: u0,
                    })
                )
                (ok true)
            )
            error (err ERR-INSUFFICIENT-FUNDS)
        )
    )
)

(define-private (update-environmental-impact
        (species (string-utf8 32))
        (region (string-utf8 32))
    )
    (let (
            (species-data (unwrap! (map-get? tree-species species) (err ERR-SPECIES-NOT-FOUND)))
            (current-regional (get-regional-impact region))
            (co2-impact (get co2-per-year species-data))
            (oxygen-impact (get oxygen-per-year species-data))
            (soil-impact (get soil-improvement species-data))
        )
        (var-set total-co2-absorbed (+ (var-get total-co2-absorbed) co2-impact))
        (var-set total-oxygen-produced
            (+ (var-get total-oxygen-produced) oxygen-impact)
        )
        (var-set total-soil-improved
            (+ (var-get total-soil-improved) soil-impact)
        )
        (map-set regional-impact region {
            trees-count: (+ (get trees-count current-regional) u1),
            co2-absorbed: (+ (get co2-absorbed current-regional) co2-impact),
            oxygen-produced: (+ (get oxygen-produced current-regional) oxygen-impact),
            soil-improved: (+ (get soil-improved current-regional) soil-impact),
        })
        (ok true)
    )
)

(define-public (register-tree-species
        (species (string-utf8 32))
        (co2-per-year uint)
        (oxygen-per-year uint)
        (soil-improvement uint)
    )
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner))
            (err ERR-NOT-AUTHORIZED)
        )
        (map-set tree-species species {
            co2-per-year: co2-per-year,
            oxygen-per-year: oxygen-per-year,
            soil-improvement: soil-improvement,
        })
        (ok true)
    )
)

(define-public (pledge-matching-funds
        (bounty-id uint)
        (amount uint)
        (match-ratio uint)
    )
    (let (
            (bounty (unwrap! (map-get? bounties bounty-id) ERR-BOUNTY-NOT-FOUND))
            (current-pool (get-bounty-matching-pool bounty-id))
        )
        (asserts! (get active bounty) ERR-INVALID-BOUNTY)
        (asserts! (> amount u0) ERR-INSUFFICIENT-FUNDS)
        (asserts! (> match-ratio u0) ERR-INVALID-BOUNTY)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set sponsor-matches {
            bounty-id: bounty-id,
            sponsor: tx-sender,
        } {
            pledged-amount: amount,
            remaining-amount: amount,
            match-ratio: match-ratio,
        })
        (map-set bounty-total-matches bounty-id {
            total-pledged: (+ (get total-pledged current-pool) amount),
            total-remaining: (+ (get total-remaining current-pool) amount),
        })
        (ok true)
    )
)

(define-public (withdraw-matching-funds (bounty-id uint))
    (let (
            (sponsor-match (unwrap!
                (map-get? sponsor-matches {
                    bounty-id: bounty-id,
                    sponsor: tx-sender,
                })
                (err ERR-NO-MATCHING-PLEDGE)
            ))
            (remaining (get remaining-amount sponsor-match))
            (current-pool (get-bounty-matching-pool bounty-id))
        )
        (asserts! (> remaining u0) (err ERR-INSUFFICIENT-FUNDS))
        (match (as-contract (stx-transfer? remaining tx-sender tx-sender))
            success (begin
                (map-set sponsor-matches {
                    bounty-id: bounty-id,
                    sponsor: tx-sender,
                } {
                    pledged-amount: (get pledged-amount sponsor-match),
                    remaining-amount: u0,
                    match-ratio: (get match-ratio sponsor-match),
                })
                (map-set bounty-total-matches bounty-id {
                    total-pledged: (get total-pledged current-pool),
                    total-remaining: (- (get total-remaining current-pool) remaining),
                })
                (ok true)
            )
            error (err ERR-INSUFFICIENT-FUNDS)
        )
    )
)

(define-private (distribute-matching-funds
        (bounty-id uint)
        (amount uint)
    )
    (let (
            (current-pool (get-bounty-matching-pool bounty-id))
            (new-remaining (- (get total-remaining current-pool) amount))
        )
        (asserts! (>= (get total-remaining current-pool) amount)
            (err ERR-INSUFFICIENT-MATCHING-FUNDS)
        )
        (map-set bounty-total-matches bounty-id {
            total-pledged: (get total-pledged current-pool),
            total-remaining: new-remaining,
        })
        (ok true)
    )
)
