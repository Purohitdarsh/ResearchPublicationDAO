;; ResearchPublication DAO Contract
;; Academic paper publishing with peer review incentives and open access funding

;; Define fungible token for DAO rewards
(define-fungible-token research-token)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-funds (err u101))
(define-constant err-paper-not-found (err u102))
(define-constant err-already-reviewed (err u103))
(define-constant err-invalid-score (err u104))
(define-constant err-not-author (err u105))

;; Paper status constants
(define-constant STATUS-SUBMITTED u1)
(define-constant STATUS-UNDER-REVIEW u2)
(define-constant STATUS-PUBLISHED u3)
(define-constant STATUS-REJECTED u4)

;; Data variables
(define-data-var paper-counter uint u0)
(define-data-var total-funding-pool uint u0)
(define-data-var review-reward uint u50) ;; Reward per review in tokens

;; Paper data structure
(define-map papers uint {
    title: (string-ascii 100),
    author: principal,
    ipfs-hash: (string-ascii 64),
    status: uint,
    review-count: uint,
    average-score: uint,
    funding-received: uint,
    submission-block: uint
})

;; Peer review tracking
(define-map reviews {paper-id: uint, reviewer: principal} {
    score: uint,
    review-hash: (string-ascii 64),
    review-block: uint
})

;; Author reputation tracking
(define-map author-reputation principal {
    papers-published: uint,
    total-citations: uint,
    reputation-score: uint
})

;; Function 1: Submit Paper for Review and Funding
(define-public (submit-paper (title (string-ascii 100)) (ipfs-hash (string-ascii 64)) (funding-request uint))
    (let ((paper-id (+ (var-get paper-counter) u1)))
        (begin
            ;; Validate input
            (asserts! (> (len title) u0) (err u106))
            (asserts! (> (len ipfs-hash) u0) (err u107))
            (asserts! (<= funding-request u10000) (err u108)) ;; Max funding cap
            
            ;; Store paper details
            (map-set papers paper-id {
                title: title,
                author: tx-sender,
                ipfs-hash: ipfs-hash,
                status: STATUS-SUBMITTED,
                review-count: u0,
                average-score: u0,
                funding-received: u0,
                submission-block: stacks-block-height
            })
            
            ;; Update paper counter
            (var-set paper-counter paper-id)
            
            ;; Initialize author reputation if first paper
            (match (map-get? author-reputation tx-sender)
                existing-rep (ok paper-id)
                (begin
                    (map-set author-reputation tx-sender {
                        papers-published: u0,
                        total-citations: u0,
                        reputation-score: u100
                    })
                    (ok paper-id)
                )
            )
        )
    )
)

;; Function 2: Submit Peer Review and Claim Rewards
(define-public (submit-review (paper-id uint) (score uint) (review-hash (string-ascii 64)))
    (let ((paper-data (unwrap! (map-get? papers paper-id) err-paper-not-found))
          (review-key {paper-id: paper-id, reviewer: tx-sender}))
        (begin
            ;; Validate review
            (asserts! (and (>= score u1) (<= score u10)) err-invalid-score)
            (asserts! (not (is-eq (get author paper-data) tx-sender)) (err u109)) ;; Can't review own paper
            (asserts! (is-none (map-get? reviews review-key)) err-already-reviewed)
            (asserts! (> (len review-hash) u0) (err u110))
            
            ;; Store review
            (map-set reviews review-key {
                score: score,
                review-hash: review-hash,
                review-block: stacks-block-height
            })
            
            ;; Update paper statistics
            (let ((new-review-count (+ (get review-count paper-data) u1))
                  (current-total-score (* (get average-score paper-data) (get review-count paper-data)))
                  (new-total-score (+ current-total-score score))
                  (new-average-score (/ new-total-score new-review-count)))
                
                (map-set papers paper-id (merge paper-data {
                    review-count: new-review-count,
                    average-score: new-average-score,
                    status: STATUS-UNDER-REVIEW
                }))
                
                ;; Mint reward tokens for reviewer
                (try! (ft-mint? research-token (var-get review-reward) tx-sender))
                
                ;; Auto-publish if paper receives 3+ reviews with average score >= 7
                (if (and (>= new-review-count u3) (>= new-average-score u7))
                    (begin
                        (map-set papers paper-id (merge paper-data {
                            review-count: new-review-count,
                            average-score: new-average-score,
                            status: STATUS-PUBLISHED
                        }))
                        
                        ;; Update author reputation
                        (match (map-get? author-reputation (get author paper-data))
                            author-rep (map-set author-reputation (get author paper-data) 
                                        (merge author-rep {
                                            papers-published: (+ (get papers-published author-rep) u1),
                                            reputation-score: (+ (get reputation-score author-rep) u50)
                                        }))
                            false
                        )
                        (ok true)
                    )
                    (ok true)
                )
            )
        )
    )
)

;; Read-only functions
(define-read-only (get-paper (paper-id uint))
    (ok (map-get? papers paper-id)))

(define-read-only (get-review (paper-id uint) (reviewer principal))
    (ok (map-get? reviews {paper-id: paper-id, reviewer: reviewer})))

(define-read-only (get-author-reputation (author principal))
    (ok (map-get? author-reputation author)))

(define-read-only (get-total-papers)
    (ok (var-get paper-counter)))

(define-read-only (get-funding-pool)
    (ok (var-get total-funding-pool)))

;; Token functions
(define-read-only (get-token-balance (account principal))
    (ok (ft-get-balance research-token account)))