# SIC/XE assembler

## Build

```
cargo build --release
```

## Usage

```
Usage: sic-xe-assembler [OPTIONS] <FILE>

Arguments:
  <FILE>  Path to source code

Options:
      --object  Print object program
  -h, --help    Print help information
```

## Examples

### Default Format

```
line   address  label     operate       operand       object code
0001         0  FIRST     STL           RETADR        17202D
0002         3            LDB           #LENGTH       69202D
0003         6            BASE          LENGTH        
0004         6  CLOOP     +JSUB         RDREC         4B101036
0005         A            LDA           LENGTH        032026
0006         D            COMP          #0            290000
0007        10            JEQ           ENDFIL        332007
0008        13            +JSUB         WRREC         4B10105D
0009        17            J             CLOOP         3F2FEC
0010        1A  ENDFIL    LDA           EOF           032010
0011        1D            STA           BUFFER        0F2016
0012        20            LDA           #3            010003
0013        23            STA           LENGTH        0F200D
0014        26            +JSUB         WRREC         4B10105D
0015        2A            J             @RETADR       3E2003
0016        2D  EOF       BYTE          C'EOF'        454F46
0017        30  RETADR    RESW          1             
0018        33  LENGTH    RESW          1             
0019        36  BUFFER    RESB          4096          
0020      1036  RDREC     CLEAR         X             B410
0021      1038            CLEAR         A             B400
0022      103A            CLEAR         S             B440
0023      103C            +LDT          #4096         75101000
0024      1040  RLOOP     TD            INPUT         E32019
0025      1043            JEQ           RLOOP         332FFA
0026      1046            RD            INPUT         DB2013
0027      1049            COMPR         A,S           A004
0028      104B            JEQ           EXIT          332008
0029      104E            STCH          BUFFER,X      57C003
0030      1051            TIXR          T             B850
0031      1053            JLT           RLOOP         3B2FEA
0032      1056  EXIT      STX           LENGTH        134000
0033      1059            RSUB                        4C0000
0034      105C  INPUT     BYTE          X'F1'         F1
0035      105D  WRREC     CLEAR         X             B410
0036      105F            LDT           LENGTH        774000
0037      1062  WLOOP     TD            OUTPUT        E32011
0038      1065            JEQ           WLOOP         332FFA
0039      1068            LDCH          BUFFER,X      53C003
0040      106B            WD            OUTPUT        DF2008
0041      106E            TIXR          T             B850
0042      1070            JLT           WLOOP         3B2FEF
0043      1073            RSUB                        4C0000
0044      1076  OUTPUT    BYTE          X'05'         05
```

### Object Code

> Use `--object` option.

```
HCOPY  000000001077
T0000001D17202D69202D4B1010360320262900003320074B10105D3F2FEC032010
T00001D130F20160100030F200D4B10105D3E2003454F46
T0010361DB410B400B44075101000E32019332FFADB2013A00433200857C003B850
T0010531D3B2FEA1340004C0000F1B410774000E32011332FFA53C003DF2008B850
T001070073B2FEF4C000005
E000000
```

## License

All source code are under MIT License. Except the asm code used for testing which is copied from *System Software: An Introduction to Systems Programming 3/e (系統程式) (導讀本)* (ISBN: 9789863780403).
