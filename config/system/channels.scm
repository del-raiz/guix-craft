;;; Define Channels
(define guix-channel
  (channel
   (name 'guix)
   (url "https://git.savannah.gnu.org/git/guix.git")
   (introduction
    (make-channel-introduction
     "9edb3f66fd807b096b48283debdcddccfea34bad"
     (openpgp-fingerprint
      "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))

(define nonguix-channel
  (channel
   (name 'nonguix)
   (url "https://gitlab.com/nonguix/nonguix")
   (introduction
    (make-channel-introduction
     "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
     (openpgp-fingerprint
      "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5")))))

(define flatwhatson-channel
  (channel
   (name 'flat)
   (url "https://github.com/flatwhatson/guix-channel.git")
   (introduction
    (make-channel-introduction
     "33f86a4b48205c0dc19d7c036c85393f0766f806"
     (openpgp-fingerprint
      "736A C00E 1254 378B A982  7AF6 9DBE 8265 81B6 4490")))))

(define guixrus-channel
  (channel
   (name 'guixrus)
   (url "https://git.sr.ht/~whereiseveryone/guixrus")
   (introduction
    (make-channel-introduction
     "7c67c3a9f299517bfc4ce8235628657898dd26b2"
     (openpgp-fingerprint
      "CD2D 5EAA A98C CB37 DA91  D6B0 5F58 1664 7F8B E551")))))

(define rde-channel
  (channel
  (name 'rde)
  (url "https://git.sr.ht/~abcdw/rde")
  (branch "master")
  (introduction
    (make-channel-introduction
      "257cebd587b66e4d865b3537a9a88cccd7107c95"
      (openpgp-fingerprint
        "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0")))))

(define aadcg-channel
  (channel
   (name 'aadcg)
   (url "https://github.com/aadcg/aadcg-guix-channel")))

;; TODO - Consider creating my own channel:
;; Ref: https://guix.gnu.org/cookbook/en/html_node/Channels.html
;; https://guix.gnu.org/manual/en/html_node/Creating-a-Channel.html#Creating-a-Channel
(define logoraz-channels
  (list nonguix-channel
        guix-channel))

;; Instantiate selected channels
(append logoraz-channels
        %default-channels)
