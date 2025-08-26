;; Options-DEX Platform Smart Contract
;; A decentralized options trading platform on Stacks blockchain

;; Define the options token for trading
(define-fungible-token options-token)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-insufficient-balance (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-option-not-found (err u104))
(define-constant err-option-expired (err u105))
(define-constant err-invalid-strike-price (err u106))

;; Data variables
(define-data-var next-option-id uint u1)
(define-data-var platform-fee-rate uint u100) ;; 1% = 100 basis points

;; Option contract structure
(define-map options
  uint
  {
    creator: principal,
    strike-price: uint,
    expiry-block: uint,
    premium: uint,
    underlying-amount: uint,
    is-call: bool,
    is-active: bool,
    buyer: (optional principal)
  })

;; User balances for collateral
(define-map user-balances principal uint)

;; Function 1: Create Option Contract
;; Allows users to create new option contracts (both calls and puts)
(define-public (create-option (strike-price uint) (expiry-blocks uint) (premium uint) (underlying-amount uint) (is-call bool))
  (let
    (
      (option-id (var-get next-option-id))
      (expiry-block (+ stacks-block-height expiry-blocks))
    )
    (begin
      ;; Validate inputs
      (asserts! (> strike-price u0) err-invalid-strike-price)
      (asserts! (> premium u0) err-invalid-amount)
      (asserts! (> underlying-amount u0) err-invalid-amount)
      (asserts! (> expiry-blocks u0) err-invalid-amount)
      
      ;; For call options, creator must deposit underlying asset as collateral
      ;; For put options, creator must deposit STX equivalent to strike price
      (if is-call
        ;; Call option: deposit underlying amount as collateral
        (try! (stx-transfer? underlying-amount tx-sender (as-contract tx-sender)))
        ;; Put option: deposit strike price * underlying amount as collateral
        (try! (stx-transfer? (* strike-price underlying-amount) tx-sender (as-contract tx-sender)))
      )
      
      ;; Create the option contract
      (map-set options option-id
        {
          creator: tx-sender,
          strike-price: strike-price,
          expiry-block: expiry-block,
          premium: premium,
          underlying-amount: underlying-amount,
          is-call: is-call,
          is-active: true,
          buyer: none
        })
      
      ;; Increment option ID for next contract
      (var-set next-option-id (+ option-id u1))
      
      ;; Return the created option ID
      (ok option-id)
    )
  ))

;; Function 2: Exercise Option
;; Allows option buyers to exercise their purchased options before expiry
(define-public (exercise-option (option-id uint))
  (let
    (
      (option-data (unwrap! (map-get? options option-id) err-option-not-found))
      (buyer (unwrap! (get buyer option-data) err-not-authorized))
    )
    (begin
      ;; Verify caller is the option buyer
      (asserts! (is-eq tx-sender buyer) err-not-authorized)
      
      ;; Check option is still active
      (asserts! (get is-active option-data) err-option-not-found)
      
      ;; Check option hasn't expired
      (asserts! (< stacks-block-height (get expiry-block option-data)) err-option-expired)
      
      (if (get is-call option-data)
        ;; Exercise call option: buyer pays strike price, gets underlying asset
        (begin
          (try! (stx-transfer? 
            (* (get strike-price option-data) (get underlying-amount option-data)) 
            tx-sender 
            (get creator option-data)))
          (try! (as-contract (stx-transfer? 
            (get underlying-amount option-data) 
            tx-sender 
            buyer)))
        )
        ;; Exercise put option: buyer gives underlying asset, gets strike price payment
        (begin
          (try! (stx-transfer? 
            (get underlying-amount option-data) 
            tx-sender 
            (as-contract tx-sender)))
          (try! (as-contract (stx-transfer? 
            (* (get strike-price option-data) (get underlying-amount option-data)) 
            tx-sender 
            buyer)))
        )
      )
      
      ;; Mark option as inactive after exercise
      (map-set options option-id
        (merge option-data { is-active: false }))
      
      (ok true)
    )
  ))

;; Read-only functions for querying option data
(define-read-only (get-option (option-id uint))
  (map-get? options option-id))

(define-read-only (get-next-option-id)
  (var-get next-option-id))

(define-read-only (get-platform-fee-rate)
  (var-get platform-fee-rate))