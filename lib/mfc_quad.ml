type vreg = [`Vreg of string]
type label = [`Label of string]

type binop =
  | Badd
  | Bsub
  | Blor
  | Bland
  | Bge
  | Bgt
  | Ble
  | Blt
  | Beq
  | Bne

type unop =
  | Ulnot
  | Ulneg

type quad =
  | Q_BINOP of binop * vreg * vreg * vreg
  | Q_IFP of vreg * int
  | Q_UNOP of unop * vreg * vreg
  | Q_SET of vreg * vreg
  | Q_SETI of vreg * int
  | Q_SETB of vreg * bool
  | Q_STR of vreg * vreg
  | Q_LD of vreg * vreg
  | Q_LABEL of label
  | Q_PUSH of vreg
  | Q_POP of vreg
  | Q_GOTO of label
  | Q_IF_GOTO of binop * vreg * vreg * label