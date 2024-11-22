Imports System.Numerics
Imports System.Threading

Public Class VCPU
    ' Definizione di una struttura per il byte ModRM
    Private Structure ModRMByte
        Dim _Mod As Byte   ' I primi 2 bit del byte ModRM
        Dim Reg As Byte    ' I 3 bit centrali del byte ModRM
        Dim RM As Byte     ' Gli ultimi 3 bit del byte ModRM
    End Structure

    ' Definizione di una struttura per il byte SIB
    Private Structure SIBByte
        Dim Scale As Byte  ' I primi 2 bit del byte SIB
        Dim Index As Byte  ' I 3 bit centrali del byte SIB
        Dim Base As Byte   ' Gli ultimi 3 bit del byte SIB
    End Structure
    Private Mem As VMem

    ' Struttura per rappresentare i prefissi dell'istruzione
    Private Structure InstructionPrefixes
        Dim AddressSizeOverride As Boolean ' Prefisso per override della dimensione dell'indirizzo
        Dim OperandSizeOverride As Boolean ' Prefisso per override della dimensione dell'operando
        Dim RepeatPrefix As Byte ' Prefisso per ripetizione: 0, &HF2, &HF3
        Dim LockPrefix As Boolean ' Prefisso per blocco
    End Structure

    Public Event Halt(sender As Object)

    'gestione degli imput esterni
    Public EnabledIRQ As Boolean
    Private _WaitBus As Boolean

    'Gestione di I/O dispositivi
    Public Event IngRQ(sender As Object, Port As UShort, size As Byte)
    Public Event OutRQ(sender As Object, Port As UShort, size As Byte)
    Private _ValoreI_O As Object

    ' Registri generali a 32 bit
    Private _EAX, _ECX, _EDX, _EBX, _ESP, _EBP, _ESI, _EDI, _IRA As UInteger
    Private InterruzioneCorrente As Boolean

    ' Registri FPU (ST0-ST7)
    Private ST0 As Double
    Private ST1 As Double
    Private ST2 As Double
    Private ST3 As Double
    Private ST4 As Double
    Private ST5 As Double
    Private ST6 As Double
    Private ST7 As Double

    ' Registri MMX (MM0-MM7)
    Private MM0 As ULong
    Private MM1 As ULong
    Private MM2 As ULong
    Private MM3 As ULong
    Private MM4 As ULong
    Private MM5 As ULong
    Private MM6 As ULong
    Private MM7 As ULong

    ' Registri XMM (XMM0-XMM7)

    Private XMM0 As Byte()
    Private XMM1 As Byte()
    Private XMM2 As Byte()
    Private XMM3 As Byte()
    Private XMM4 As Byte()
    Private XMM5 As Byte()
    Private XMM6 As Byte()
    Private XMM7 As Byte()

    ' Registro dei flag
    ' EFLAGS register
    Private _EFLAGS As UInteger

    ' Flag constants
    Private Const CF As UInteger = 1 << 0  ' Carry Flag
    Private Const PF As UInteger = 1 << 2  ' Parity Flag
    Private Const AF As UInteger = 1 << 4  ' Auxiliary Carry Flag
    Private Const ZF As UInteger = 1 << 6  ' Zero Flag
    Private Const SF As UInteger = 1 << 7  ' Sign Flag
    Private Const _If As UInteger = 1 << 9  ' Interrupt Enable Flag
    Private Const DF As UInteger = 1 << 10 ' Direction Flag
    Private Const _Of As UInteger = 1 << 11 ' Overflow Flag

    Private prefixes As InstructionPrefixes

    ' Contatore del programma
    Private EIP As UInteger

    Public Sub New(value As VMem, Optional EIORQ As Boolean = True)
        Mem = value

        ' Inizializza i registri
        ValoreI_O = 0
        EnabledIRQ = EIORQ
        _WaitBus = False
        InterruzioneCorrente = False
        EAX = 0 : ECX = 0 : EDX = 0 : EBX = 0
        ESP = Mem.Memory_size : EBP = 0 : ESI = 0 : EDI = 0
        IRA = 0
        ST0 = 0
        ST1 = 0
        ST2 = 0
        ST7 = 0
        ST6 = 0
        ST5 = 0
        ST4 = 0
        ST3 = 0

        MM0 = 0
        MM1 = 0
        MM2 = 0
        MM3 = 0
        MM4 = 0
        MM5 = 0
        MM6 = 0
        MM7 = 0

        XMM0 = New Byte(15) {}
        XMM1 = New Byte(15) {}
        XMM2 = New Byte(15) {}
        XMM3 = New Byte(15) {}
        XMM4 = New Byte(15) {}
        XMM5 = New Byte(15) {}
        XMM6 = New Byte(15) {}
        XMM7 = New Byte(15) {}

        _EFLAGS = 0
        EIP = 0

    End Sub

    Public Sub Reset()

        ' Inizializza i registri
        ValoreI_O = 0
        _WaitBus = False
        InterruzioneCorrente = False
        EAX = 0 : ECX = 0 : EDX = 0 : EBX = 0
        ESP = Mem.Memory_size : EBP = 0 : ESI = 0 : EDI = 0
        IRA = 0
        ST0 = 0
        ST1 = 0
        ST2 = 0
        ST7 = 0
        ST6 = 0
        ST5 = 0
        ST4 = 0
        ST3 = 0

        MM0 = 0
        MM1 = 0
        MM2 = 0
        MM3 = 0
        MM4 = 0
        MM5 = 0
        MM6 = 0
        MM7 = 0

        XMM0 = New Byte(15) {}
        XMM1 = New Byte(15) {}
        XMM2 = New Byte(15) {}
        XMM3 = New Byte(15) {}
        XMM4 = New Byte(15) {}
        XMM5 = New Byte(15) {}
        XMM6 = New Byte(15) {}
        XMM7 = New Byte(15) {}

        _EFLAGS = 0
        EIP = 0

    End Sub

    ' Registri a 16 bit (parte bassa dei registri a 32 bit)
    Private Property AX() As UShort
        Get
            Return CUShort(_EAX And &HFFFF)
        End Get
        Set(value As UShort)
            _EAX = (_EAX And &HFFFF0000UI) Or value
        End Set
    End Property
    Private Property CX() As UShort
        Get
            Return CUShort(_ECX And &HFFFF)
        End Get
        Set(value As UShort)
            _ECX = (_ECX And &HFFFF0000UI) Or value
        End Set
    End Property
    Private Property DX() As UShort
        Get
            Return CUShort(_EDX And &HFFFF)
        End Get
        Set(value As UShort)
            _EDX = (_EDX And &HFFFF0000UI) Or value
        End Set
    End Property
    Private Property BX() As UShort
        Get
            Return CUShort(_EBX And &HFFFF)
        End Get
        Set(value As UShort)
            _EBX = (_EBX And &HFFFF0000UI) Or value
        End Set
    End Property
    Private Property SP() As UShort
        Get
            Return CUShort(_ESP And &HFFFF)
        End Get
        Set(value As UShort)
            _ESP = (_ESP And &HFFFF0000UI) Or value
        End Set
    End Property
    Private Property BP() As UShort
        Get
            Return CUShort(_EBP And &HFFFF)
        End Get
        Set(value As UShort)
            _EBP = (_EBP And &HFFFF0000UI) Or value
        End Set
    End Property
    Private Property SI() As UShort
        Get
            Return CUShort(_ESI And &HFFFF)
        End Get
        Set(value As UShort)
            _ESI = (_ESI And &HFFFF0000UI) Or value
        End Set
    End Property
    Private Property DI() As UShort
        Get
            Return CUShort(_EDI And &HFFFF)
        End Get
        Set(value As UShort)
            _EDI = (_EDI And &HFFFF0000UI) Or value
        End Set
    End Property

    ' Registri a 8 bit (parte bassa e alta dei registri a 16 bit)
    Private Property AL() As Byte
        Get
            Return CByte(_EAX And &HFF)
        End Get
        Set(value As Byte)
            _EAX = (_EAX And &HFFFFFF00UI) Or value
        End Set
    End Property
    Private Property CL() As Byte
        Get
            Return CByte(_ECX And &HFF)
        End Get
        Set(value As Byte)
            _ECX = (_ECX And &HFFFFFF00UI) Or value
        End Set
    End Property
    Private Property DL() As Byte
        Get
            Return CByte(_EDX And &HFF)
        End Get
        Set(value As Byte)
            _EDX = (_EDX And &HFFFFFF00UI) Or value
        End Set
    End Property
    Private Property BL() As Byte
        Get
            Return CByte(_EBX And &HFF)
        End Get
        Set(value As Byte)
            _EBX = (_EBX And &HFFFFFF00UI) Or value
        End Set
    End Property
    Private Property AH() As Byte
        Get
            Return CByte((_EAX >> 8) And &HFF)
        End Get
        Set(value As Byte)
            _EAX = (_EAX And &HFFFF00FFUI) Or (CUInt(value) << 8)
        End Set
    End Property
    Private Property CH() As Byte
        Get
            Return CByte((_ECX >> 8) And &HFF)
        End Get
        Set(value As Byte)
            _ECX = (_ECX And &HFFFF00FFUI) Or (CUInt(value) << 8)
        End Set
    End Property
    Private Property DH() As Byte
        Get
            Return CByte((_EDX >> 8) And &HFF)
        End Get
        Set(value As Byte)
            _EDX = (_EDX And &HFFFF00FFUI) Or (CUInt(value) << 8)
        End Set
    End Property
    Private Property BH() As Byte
        Get
            Return CByte((_EBX >> 8) And &HFF)
        End Get
        Set(value As Byte)
            _EBX = (_EBX And &HFFFF00FFUI) Or (CUInt(value) << 8)
        End Set
    End Property
    Private Property IRA() As UInteger
        Get
            Return _IRA
        End Get
        Set(value As UInteger)
            _IRA = value
        End Set
    End Property

    Private Property EAX() As UInteger
        Get
            Return _EAX
        End Get
        Set(value As UInteger)
            _EAX = value
        End Set
    End Property
    Private Property ECX() As UInteger
        Get
            Return _ECX
        End Get
        Set(value As UInteger)
            _ECX = value
        End Set
    End Property
    Private Property EDX() As UInteger
        Get
            Return _EDX
        End Get
        Set(value As UInteger)
            _EDX = value
        End Set
    End Property
    Private Property EBX() As UInteger
        Get
            Return _EBX
        End Get
        Set(value As UInteger)
            _EBX = value
        End Set
    End Property
    Private Property ESP() As UInteger
        Get
            Return _ESP
        End Get
        Set(value As UInteger)
            _ESP = value
        End Set
    End Property
    Private Property EBP() As UInteger
        Get
            Return _EBP
        End Get
        Set(value As UInteger)
            _EBP = value
        End Set
    End Property
    Private Property ESI() As UInteger
        Get
            Return _ESI
        End Get
        Set(value As UInteger)
            _ESI = value
        End Set
    End Property
    Private Property EDI() As UInteger
        Get
            Return _EDI
        End Get
        Set(value As UInteger)
            _EDI = value
        End Set
    End Property
    ' Properties for each flag
    Private Property CarryFlag As Boolean
        Get
            Return (_EFLAGS And CF) <> 0
        End Get
        Set(value As Boolean)
            _EFLAGS = If(value, _EFLAGS Or CF, _EFLAGS And Not CF)
        End Set
    End Property

    Private Property ParityFlag As Boolean
        Get
            Return (_EFLAGS And PF) <> 0
        End Get
        Set(value As Boolean)
            _EFLAGS = If(value, _EFLAGS Or PF, _EFLAGS And Not PF)
        End Set
    End Property

    Private Property AuxiliaryCarryFlag As Boolean
        Get
            Return (_EFLAGS And AF) <> 0
        End Get
        Set(value As Boolean)
            _EFLAGS = If(value, _EFLAGS Or AF, _EFLAGS And Not AF)
        End Set
    End Property

    Private Property ZeroFlag As Boolean
        Get
            Return (_EFLAGS And ZF) <> 0
        End Get
        Set(value As Boolean)
            _EFLAGS = If(value, _EFLAGS Or ZF, _EFLAGS And Not ZF)
        End Set
    End Property

    Private Property SignFlag As Boolean
        Get
            Return (_EFLAGS And SF) <> 0
        End Get
        Set(value As Boolean)
            _EFLAGS = If(value, _EFLAGS Or SF, _EFLAGS And Not SF)
        End Set
    End Property

    Private Property OverflowFlag As Boolean
        Get
            Return (_EFLAGS And _Of) <> 0
        End Get
        Set(value As Boolean)
            _EFLAGS = If(value, _EFLAGS Or _Of, _EFLAGS And Not _Of)
        End Set
    End Property

    Private Property DirectionFlag As Boolean
        Get
            Return (_EFLAGS And DF) <> 0
        End Get
        Set(value As Boolean)
            _EFLAGS = If(value, _EFLAGS Or DF, _EFLAGS And Not DF)
        End Set
    End Property

    Private Property InterruptFlag As Boolean
        Get
            Return (_EFLAGS And _If) <> 0
        End Get
        Set(value As Boolean)
            _EFLAGS = If(value, _EFLAGS Or _If, _EFLAGS And Not _If)
        End Set
    End Property

    Public Property ValoreI_O As Object
        Get
            Return _ValoreI_O
        End Get
        Set(value As Object)
            _ValoreI_O = value
        End Set
    End Property

    Public Sub IrqAsk()
        InterruptFlag = True
    End Sub

    Public Property WaitBus As Boolean
        Get
            Return _WaitBus
        End Get
        Set(value As Boolean)
            _WaitBus = value
        End Set
    End Property

    Private Sub SetFlagsAfterLogicalOperation(result As UInteger, size As Integer)
        ZeroFlag = (result = 0)
        SignFlag = ((result >> (size - 1)) And 1) <> 0
        CarryFlag = False
        OverflowFlag = False
        ParityFlag = HasEvenParity(CByte(result And &HFF))
    End Sub

    ' Funzione per impostare i flag dopo un'operazione
    Private Sub SetFlagsAfterOperation(result As UInteger, size As Integer)
        ' Imposta ZF (Zero Flag)
        ZeroFlag = (result = 0)

        ' Imposta SF (Sign Flag)
        SignFlag = ((result >> (size - 1)) And 1) <> 0

        ' Imposta PF (Parity Flag)
        Dim lsb As Byte = CByte(result And &HFF)
        ParityFlag = HasEvenParity(lsb)

        ' Nota: CF (Carry Flag) e OF (Overflow Flag) dipendono dall'operazione specifica
        ' e dovrebbero essere impostati nelle singole implementazioni delle istruzioni
    End Sub

    Private Sub SetFlagsAfterIncDec(result As UInteger, size As Integer, isIncrement As Boolean)
        ' Imposta ZF (Zero Flag)
        ZeroFlag = (result = 0)

        ' Imposta SF (Sign Flag)
        SignFlag = ((result >> (size - 1)) And 1) <> 0

        ' Imposta PF (Parity Flag)
        Dim lsb As Byte = CByte(result And &HFF)
        ParityFlag = HasEvenParity(lsb)

        ' Imposta OF (Overflow Flag)
        Select Case size
            Case 8
                OverflowFlag = (result = &H80UI) Xor (Not isIncrement)
            Case 16
                OverflowFlag = (result = &H8000UI) Xor (Not isIncrement)
            Case 32
                OverflowFlag = (result = &H80000000UI) Xor (Not isIncrement)
        End Select

        ' Imposta AF (Auxiliary Carry Flag)
        AuxiliaryCarryFlag = (result And &HF) = 0 Xor (Not isIncrement)

        ' Nota: CF (Carry Flag) non viene modificato da INC/DEC
    End Sub

    Private Sub SetFlagsAfterIMUL(result As Integer, mask As UInteger)
        ' Per IMUL, solo CF e OF sono definiti, gli altri flag sono indefiniti

        ' Calcola il risultato effettivo a 16 bit
        Dim signedResult As Short = CShort(result And mask)

        ' Verifica se c'è stata una perdita di precisione
        Dim hasLostPrecision As Boolean = (CInt(signedResult) <> result)

        ' Imposta CF e OF
        CarryFlag = hasLostPrecision
        OverflowFlag = hasLostPrecision

        ' Gli altri flag (ZF, SF, PF, AF) sono indefiniti dopo IMUL
        ' In un emulatore, potremmo lasciarli invariati o impostarli in modo casuale
        ' Per coerenza, li lasceremo invariati
    End Sub

    Private Sub SetFPUFlags(result As Double)
        ' Imposta Zero Flag (ZF)
        ZeroFlag = (result = 0)

        ' Imposta Sign Flag (SF)
        SignFlag = (result < 0)

        ' Imposta Carry Flag (CF)
        ' Nel contesto FPU, CF è spesso usato per indicare condizioni speciali
        CarryFlag = Double.IsNaN(result) OrElse Double.IsInfinity(result)

        ' Imposta Overflow Flag (OF)
        ' Nel contesto FPU, OF è generalmente impostato per overflow o operazioni non valide
        OverflowFlag = Double.IsInfinity(result)

        ' Imposta Parity Flag (PF)
        ' Nel contesto FPU, PF è spesso usato per indicare la precisione
        ' Qui, lo impostiamo se il risultato è un numero finito (non NaN o infinito)
        ParityFlag = Not (Double.IsNaN(result) OrElse Double.IsInfinity(result))

        ' Auxiliary Carry Flag (AF) non è generalmente usato nelle operazioni FPU
        ' Lo lasciamo invariato

        ' Direction Flag (DF) non è rilevante per le operazioni FPU
        ' Lo lasciamo invariato

        ' Interrupt Flag (IF) non è influenzato dalle operazioni FPU
        ' Lo lasciamo invariato
    End Sub

    ' Funzione helper per verificare se un risultato è denormalizzato
    Private Function IsDenormalized(value As Double) As Boolean
        Dim bits As Long = BitConverter.DoubleToInt64Bits(value)
        Dim exponent As Integer = CInt((bits >> 52) And &H7FF)
        Dim mantissa As Long = bits And &HFFFFFFFFFFFFFUL

        Return exponent = 0 AndAlso mantissa <> 0
    End Function

    ' Funzione per decodificare i prefissi dell'istruzione
    Private Sub DecodePrefixes()
        Dim prefix As Byte

        ' Ciclo per leggere e interpretare i prefissi
        Do
            prefix = Mem.ReadByte(EIP)
            Select Case prefix
                Case &H66 : prefixes.OperandSizeOverride = True ' Override della dimensione dell'operando
                Case &H67 : prefixes.AddressSizeOverride = True ' Override della dimensione dell'indirizzo
                Case &HF2, &HF3 : prefixes.RepeatPrefix = prefix ' Prefissi di ripetizione
                Case Else : Exit Do ' Esci dal ciclo se non è un prefisso valido
            End Select
            EIP += 1 ' Incrementa il puntatore dell'istruzione
        Loop

    End Sub

    ' Funzione per leggere un registro a 8 bit
    Private Function ReadRegister8(ByVal regCode As Byte) As Byte
        Select Case regCode
            Case 0 : Return AL ' Registro AL
            Case 1 : Return CL ' Registro CL
            Case 2 : Return DL ' Registro DL
            Case 3 : Return BL ' Registro BL
            Case 4 : Return AH ' Registro AH
            Case 5 : Return CH ' Registro CH
            Case 6 : Return DH ' Registro DH
            Case 7 : Return BH ' Registro BH
            Case Else : LogError("Codice registro non valido")
        End Select
    End Function

    ' Funzione per scrivere in un registro a 8 bit
    Private Sub WriteRegister8(ByVal regCode As Byte, ByVal value As Byte)
        Select Case regCode
            Case 0 : AL = value ' Registro AL
            Case 1 : CL = value ' Registro CL
            Case 2 : DL = value ' Registro DL
            Case 3 : BL = value ' Registro BL
            Case 4 : AH = value ' Registro AH
            Case 5 : CH = value ' Registro CH
            Case 6 : DH = value ' Registro DH
            Case 7 : BH = value ' Registro BH
            Case Else : LogError("Codice registro non valido")
        End Select
    End Sub

    ' Funzione per leggere un registro a 16 bit
    Private Function ReadRegister16(ByVal regCode As Byte) As UShort
        Select Case regCode
            Case 0 : Return AX ' Registro AX
            Case 1 : Return CX ' Registro CX
            Case 2 : Return DX ' Registro DX
            Case 3 : Return BX ' Registro BX
            Case 4 : Return SP ' Registro SP
            Case 5 : Return BP ' Registro BP
            Case 6 : Return SI ' Registro SI
            Case 7 : Return DI ' Registro DI
            Case Else : LogError("Codice registro non valido")
        End Select
    End Function

    ' Funzione per scrivere in un registro a 16 bit
    Private Sub WriteRegister16(ByVal regCode As Byte, ByVal value As UShort)
        Select Case regCode
            Case 0 : AX = value ' Registro AX
            Case 1 : CX = value ' Registro CX
            Case 2 : DX = value ' Registro DX
            Case 3 : BX = value ' Registro BX
            Case 4 : SP = value ' Registro SP
            Case 5 : BP = value ' Registro BP
            Case 6 : SI = value ' Registro SI
            Case 7 : DI = value ' Registro DI
            Case Else : LogError("Codice registro non valido")
        End Select
    End Sub

    ' Funzione per leggere un registro a 32 bit
    Private Function ReadRegister32(ByVal regCode As Byte) As UInteger
        Select Case regCode
            Case 0 : Return EAX ' Registro EAX
            Case 1 : Return ECX ' Registro ECX
            Case 2 : Return EDX ' Registro EDX
            Case 3 : Return EBX ' Registro EBX
            Case 4 : Return ESP ' Registro ESP
            Case 5 : Return EBP ' Registro EBP
            Case 6 : Return ESI ' Registro ESI
            Case 7 : Return EDI ' Registro EDI
            Case Else : LogError("Codice registro non valido")
        End Select
    End Function

    ' Funzione per scrivere in un registro a 32 bit
    Private Sub WriteRegister32(ByVal regCode As Byte, ByVal value As UInteger)
        Select Case regCode
            Case 0 : EAX = value ' Registro EAX
            Case 1 : ECX = value ' Registro ECX
            Case 2 : EDX = value ' Registro EDX
            Case 3 : EBX = value ' Registro EBX
            Case 4 : ESP = value ' Registro ESP
            Case 5 : EBP = value ' Registro EBP
            Case 6 : ESI = value ' Registro ESI
            Case 7 : EDI = value ' Registro EDI
            Case Else : LogError("Codice registro non valido")
        End Select
    End Sub

    ' Funzione per leggere un registro a 64 bit MMX
    Private Function ReadMMX(ByVal regCode As Byte) As ULong
        Select Case regCode
            Case 0 : Return MM0 ' Registro MM0
            Case 1 : Return MM1 ' Registro MM1
            Case 2 : Return MM2 ' Registro MM2
            Case 3 : Return MM3 ' Registro MM3
            Case 4 : Return MM4 ' Registro MM4
            Case 5 : Return MM5 ' Registro MM5
            Case 6 : Return MM6 ' Registro MM6
            Case 7 : Return MM7 ' Registro MM7
            Case Else : LogError("Codice registro non valido")
        End Select
    End Function

    ' Funzione per scrivere in un registro a 64 bit MMX
    Private Sub WriteMMX(ByVal regCode As Byte, ByVal value As ULong)
        Select Case regCode
            Case 0 : MM0 = value ' Registro MM0
            Case 1 : MM1 = value ' Registro MM1
            Case 2 : MM2 = value ' Registro MM2
            Case 3 : MM3 = value ' Registro MM3
            Case 4 : MM4 = value ' Registro MM4
            Case 5 : MM5 = value ' Registro MM5
            Case 6 : MM6 = value ' Registro MM6
            Case 7 : MM7 = value ' Registro MM7
            Case Else : LogError("Codice registro non valido")
        End Select
    End Sub

    ' Funzione per leggere un registro a 128 bit XMM
    Private Function ReadXMM(ByVal regCode As Byte) As Byte()
        Select Case regCode
            Case 0 : Return XMM0 ' Registro XMM0
            Case 1 : Return XMM1 ' Registro XMM1
            Case 2 : Return XMM2 ' Registro XMM2
            Case 3 : Return XMM3 ' Registro XMM3
            Case 4 : Return XMM4 ' Registro XMM4
            Case 5 : Return XMM5 ' Registro XMM5
            Case 6 : Return XMM6 ' Registro XMM6
            Case 7 : Return XMM7 ' Registro XMM7
            Case Else : LogError("Codice registro non valido")
        End Select
    End Function

    ' Funzione per scrivere in un registro a 128 bit XMM
    Private Sub WriteXMM(ByVal regCode As Byte, ByVal value As Byte())
        Select Case regCode
            Case 0 : XMM0 = value ' Registro XMM0
            Case 1 : XMM1 = value ' Registro XMM1
            Case 2 : XMM2 = value ' Registro XMM2
            Case 3 : XMM3 = value ' Registro XMM3
            Case 4 : XMM4 = value ' Registro XMM4
            Case 5 : XMM5 = value ' Registro XMM5
            Case 6 : XMM6 = value ' Registro XMM6
            Case 7 : XMM7 = value ' Registro XMM7
            Case Else : LogError("Codice registro non valido")
        End Select
    End Sub

    '
    Private Function ReadSTRegister(index As Byte) As Double
        Select Case index
            Case 0 : Return ST0
            Case 1 : Return ST1
            Case 2 : Return ST2
            Case 3 : Return ST3
            Case 4 : Return ST4
            Case 5 : Return ST5
            Case 6 : Return ST6
            Case 7 : Return ST7
            Case Else
                logerror("Indice registro ST non valido: " & index)
        End Select
    End Function

    Private Sub WriteSTRegister(index As Byte, value As Double)
        Select Case index
            Case 0 : ST0 = value
            Case 1 : ST1 = value
            Case 2 : ST2 = value
            Case 3 : ST3 = value
            Case 4 : ST4 = value
            Case 5 : ST5 = value
            Case 6 : ST6 = value
            Case 7 : ST7 = value
            Case Else
                logerror("Indice registro ST non valido: " & index)
        End Select
    End Sub

    ' Funzione helper per ruotare lo stack FPU
    Private Sub RotateFPUStack(direction As Integer)
        If direction > 0 Then
            ' Rotazione verso il basso (push)
            Dim temp As Double = ST7
            ST7 = ST6
            ST6 = ST5
            ST5 = ST4
            ST4 = ST3
            ST3 = ST2
            ST2 = ST1
            ST1 = ST0
            ST0 = temp
        ElseIf direction < 0 Then
            ' Rotazione verso l'alto (pop)
            Dim temp As Double = ST0
            ST0 = ST1
            ST1 = ST2
            ST2 = ST3
            ST3 = ST4
            ST4 = ST5
            ST5 = ST6
            ST6 = ST7
            ST7 = temp
        End If
    End Sub

    ' Funzione per push di un valore nello stack FPU
    Private Sub PushFPUStack(value As Double)
        RotateFPUStack(1)
        ST0 = value
    End Sub

    ' Funzione per pop di un valore dallo stack FPU
    Private Function PopFPUStack() As Double
        Dim value As Double = ST0
        RotateFPUStack(-1)
        Return value
    End Function

    Private Function GetSizeMask(size As Integer) As Integer
        Select Case size
            Case 8
                Return &HFF
            Case 16
                Return &HFFFF
            Case 32
                Return &HFFFFFFFF
            Case Else
                logerror("Invalid size")
        End Select
    End Function

    ' Funzione per controllare la parità di un valore
    Private Function HasEvenParity(value As Byte) As Boolean
        Dim count As Integer = 0
        While value <> 0
            count += value And 1
            value >>= 1
        End While
        Return (count Mod 2) = 0
    End Function

    ' Funzione per ottenere il valore completo di EFLAGS
    Private Function GetEFLAGS() As UInteger
        Return _EFLAGS
    End Function

    ' Funzione per impostare il valore completo di EFLAGS
    Private Sub SetEFLAGS(value As UInteger)
        _EFLAGS = value
    End Sub

    ' Funzione per decodificare il byte ModRM
    Private Function DecodeModRM() As ModRMByte
        Dim modRMByte As Byte = Mem.ReadByte(EIP) ' Legge un byte dalla memoria
        EIP += 1 ' Incrementa il puntatore dell'istruzione (EIP)
        ' Decodifica i campi _Mod, Reg, e RM dal byte ModRM e ritorna una struttura ModRMByte
        Return New ModRMByte With {
            ._Mod = CByte((modRMByte >> 6) And 3), ' Estrae i primi 2 bit
            .Reg = CByte((modRMByte >> 3) And 7),  ' Estrae i 3 bit centrali
            .RM = CByte(modRMByte And 7)           ' Estrae gli ultimi 3 bit
        }
    End Function

    ' Funzione per decodificare il byte SIB
    Private Function DecodeSIB() As SIBByte
        Dim sibByte As Byte = Mem.ReadByte(EIP) ' Legge un byte dalla memoria
        EIP += 1 ' Incrementa il puntatore dell'istruzione (EIP)
        ' Decodifica i campi Scale, Index, e Base dal byte SIB e ritorna una struttura SIBByte
        Return New SIBByte With {
            .Scale = CByte((sibByte >> 6) And 3), ' Estrae i primi 2 bit
            .Index = CByte((sibByte >> 3) And 7), ' Estrae i 3 bit centrali
            .Base = CByte(sibByte And 7)          ' Estrae gli ultimi 3 bit
        }
    End Function

    ' Funzione per decodificare il displacement in base ai bit Mod e alla dimensione dell'indirizzo
    Private Function DecodeDisplacement(modBits As Byte, addressSize As Integer) As Integer
        If addressSize = 16 Then ' Se la dimensione dell'indirizzo è 16-bit
            Select Case modBits
                Case 0
                    Return 0 ' Nessun displacement
                Case 1
                    Return CSByte(Mem.ReadByte(EIP)) ' 8-bit displacement
                Case 2
                    Return CShort(Mem.ReadWord(EIP)) ' 16-bit displacement
                Case Else
                    LogError("Invalid mod bits for 16-bit addressing")
            End Select
        Else ' 32-bit addressing
            Select Case modBits
                Case 0
                    Return 0 ' Nessun displacement
                Case 1
                    Return CSByte(Mem.ReadByte(EIP)) ' 8-bit displacement
                Case 2
                    Return CInt(Mem.ReadDWord(EIP)) ' 32-bit displacement
                Case Else
                    LogError("Invalid mod bits for 32-bit addressing")
            End Select
        End If
    End Function

    ' Funzione per decodificare l'indirizzo effettivo in base al byte ModRM e ai prefissi dell'istruzione
    Private Function DecodeEffectiveAddress(modRM As ModRMByte) As UInteger
        Dim addressSize As Integer = If(prefixes.AddressSizeOverride, 16, 32) ' Determina la dimensione dell'indirizzo
        Dim effectiveAddress As UInteger = 0

        If addressSize = 16 Then ' Logica per indirizzamento a 16-bit
            Select Case modRM.RM
                Case 0 To 3
                    effectiveAddress = ReadRegister16(modRM.RM) ' Registro
                Case 4
                    effectiveAddress = ReadRegister16(2) ' SI register
                Case 5
                    effectiveAddress = ReadRegister16(3) ' DI register
                Case 6
                    If modRM._Mod = 0 Then
                        effectiveAddress = Mem.ReadWord(EIP) ' Valore diretto dalla memoria
                        EIP += 2
                    Else
                        effectiveAddress = ReadRegister16(5) ' BP register
                    End If
                Case 7
                    effectiveAddress = ReadRegister16(6) ' BX register
            End Select
        Else ' Logica per indirizzamento a 32-bit
            If modRM.RM = 4 Then
                Dim sib As SIBByte = DecodeSIB() ' Presenza di byte SIB
                effectiveAddress = DecodeSIBAddress(sib, modRM._Mod)
            ElseIf modRM.RM = 5 AndAlso modRM._Mod = 0 Then
                effectiveAddress = Mem.ReadDWord(EIP) ' Displacement a 32-bit
                EIP += 4
            Else
                effectiveAddress = ReadRegister32(modRM.RM) ' Registro a 32-bit
            End If
        End If

        ' Aggiunge il displacement se presente
        Dim displacement As Integer = DecodeDisplacement(modRM._Mod, addressSize)
        effectiveAddress += CUInt(displacement)
        ' Aggiorna il puntatore dell'istruzione in base ai bit Mod
        EIP += If(modRM._Mod = 1, 1, If(modRM._Mod = 2, If(addressSize = 16, 2, 4), 0))

        Return effectiveAddress
    End Function

    ' Funzione per decodificare l'indirizzo con il byte SIB
    Private Function DecodeSIBAddress(sib As SIBByte, modBits As Byte) As UInteger
        Dim base As UInteger = If(sib.Base = 5 AndAlso modBits = 0, Mem.ReadDWord(EIP), ReadRegister32(sib.Base)) ' Calcola la base
        Dim index As UInteger = If(sib.Index = 4, 0, ReadRegister32(sib.Index)) ' Calcola l'indice
        Dim scale As Integer = 1 << sib.Scale ' Calcola il fattore di scala

        If sib.Base = 5 AndAlso modBits = 0 Then
            EIP += 4 ' Aggiorna EIP se la base è 5 e modBits è 0
        End If

        Return base + (index * CUInt(scale)) ' Ritorna l'indirizzo effettivo calcolato
    End Function


    Public Sub ExecuteInstruction()
        If WaitBus = True Then
            ExecuteNOP()
        Else
            DecodePrefixes()
            Dim opcode As Byte = Mem.ReadByte(EIP)
            EIP += 1

            Select Case opcode
        ' Istruzioni specifiche
                Case &H27 : ExecuteDAA()
                Case &H2F : ExecuteDAS()
                Case &H37 : ExecuteAAA()
                Case &H3F : ExecuteAAS()
                Case &H60 : ExecutePUSHA()
                Case &H61 : ExecutePOPA()
                Case &H62 : ExecuteBOUND()
                Case &H90 : ExecuteNOP()
                Case &H98 : ExecuteCWDE()
                Case &H99 : ExecuteCDQ()
                Case &H9C : ExecutePUSHF()
                Case &H9D : ExecutePOPF()
                Case &H9E : ExecuteSAHF()
                Case &H9F : ExecuteLAHF()
                Case &HF4 : ExecuteHLT()
                Case &HF5 : ExecuteCMC()
                Case &HF8 : ExecuteCLC()
                Case &HF9 : ExecuteSTC()
                Case &HFA : ExecuteCLi()
                Case &HFB : ExecuteSTI()
                Case &HFC : ExecuteCLD()
                Case &HFD : ExecuteSTD()

        ' Istruzioni aritmetiche e logiche di base
                Case &H0, &H1, &H2, &H3, &H4, &H5 : ExecuteADD(opcode)
                Case &H8, &H9, &HA, &HB, &HC, &HD : ExecuteOR(opcode)
                Case &H10, &H11, &H12, &H13, &H14, &H15 : ExecuteADC(opcode)
                Case &H18, &H19, &H1A, &H1B, &H1C, &H1D : ExecuteSBB(opcode)
                Case &H20, &H21, &H22, &H23, &H24, &H25 : ExecuteAND(opcode)
                Case &H28, &H29, &H2A, &H2B, &H2C, &H2D : ExecuteSUB(opcode)
                Case &H30, &H31, &H32, &H33, &H34, &H35 : ExecuteXOR(opcode)
                Case &H38, &H39, &H3A, &H3B, &H3C, &H3D : ExecuteCMP(opcode)

        ' Istruzioni INC/DEC
                Case &H40 To &H47 : ExecuteINC(opcode)
                Case &H48 To &H4F : ExecuteDEC(opcode)

        ' Istruzioni PUSH/POP
                Case &H50 To &H57, &H68, &H6A : ExecutePUSH(opcode)
                Case &H58 To &H5F, &H8F : ExecutePOP(opcode)

        ' Altre istruzioni specifiche
                Case &H69, &H6B : ExecuteIMUL(opcode)
                Case &H6C, &H6D : ExecuteINS(opcode)
                Case &H6E, &H6F : ExecuteOUTS(opcode)

        ' Salti condizionali
                Case &H70 To &H7F, &HE3 : ExecuteJcc(opcode)

        ' Istruzioni aritmetiche e logiche immediate
                Case &H80 To &H83
                    ExecuteArithmeticImmediate(opcode)

        ' Istruzioni TEST e XCHG
                Case &H84, &H85, &HA8, &HA9 : ExecuteTEST(opcode)
                Case &H86, &H87, &H91 To &H97 : ExecuteXCHG(opcode)

        ' Istruzioni MOV
                Case &H88, &H89, &H8A, &H8B, &H8C, &H8E, &HA0, &HA1, &HA2, &HA3, &HB0 To &HBF, &HC6, &HC7 : ExecuteMOV(opcode)

        ' Altre istruzioni di memoria e stringhe
                Case &H8D : ExecuteLEA()
                Case &H9A, &HE8 : ExecuteCALL(opcode)
                Case &HA4, &HA5 : ExecuteMOVS(opcode)
                Case &HA6, &HA7 : ExecuteCMPS(opcode)
                Case &HAA, &HAB : ExecuteSTOS(opcode)
                Case &HAC, &HAD : ExecuteLODS(opcode)
                Case &HAE, &HAF : ExecuteSCAS(opcode)

        ' Istruzioni di shift e rotazione
                Case &HC0, &HC1, &HD0 To &HD3
                    ExecuteShiftRotate(opcode)

        ' Istruzioni di ritorno
                Case &HC2, &HC3, &HCA, &HCB : ExecuteRET(opcode)
                Case &HCF : ExecuteIRET()

        ' Istruzioni aritmetiche ASCII
                Case &HD4 : ExecuteAAM()
                Case &HD5 : ExecuteAAD()
                Case &HD7 : ExecuteXLAT()

        ' Istruzioni FPU
                Case &HD8 To &HDF : ExecuteFPUInstruction(opcode)

        ' Istruzioni di loop
                Case &HE0, &HE1, &HE2 : ExecuteLOOP(opcode)

        ' Istruzioni I/O
                Case &HE4, &HE5, &HEC, &HED : ExecuteIN(opcode)
                Case &HE6, &HE7, &HEE, &HEF : ExecuteOUT(opcode)

        ' Istruzioni di salto
                Case &HE9, &HEA, &HEB : ExecuteJMP(opcode)

        ' Istruzioni con prefisso di escape
                Case &HF
                    ExecuteExtendedInstructions()

        ' Istruzioni NOT, NEG, MUL, IMUL, DIV, IDIV
                Case &HF6, &HF7
                    ExecuteGroupF6F7(opcode)

        ' Istruzioni INC, DEC, CALL, JMP
                Case &HFE, &HFF
                    ExecuteGroupFEFF(opcode)
            End Select

            'HandleInterrupt
            If InterruptFlag And Not InterruzioneCorrente And EnabledIRQ Then
                'attiva interruzione corrente in modo da bloccare altre gestioni di interruzione
                InterruzioneCorrente = True
                ' Salva lo stato corrente
                'Push(EFLAGS)
                ESP -= 4
                Mem.WriteDWord(ESP, _EFLAGS)
                ' Push(EIP)
                ESP -= 4
                Mem.WriteDWord(ESP, EIP)
                ' Carica il nuovo EIP
                EIP = IRA
            End If
        End If
    End Sub

    Private Sub ExecuteArithmeticImmediate(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()
        Select Case opcode
            Case &H80 : ExecuteArithmeticImmediate8(modRM)
            Case &H81
                If prefixes.OperandSizeOverride Then
                    ExecuteArithmeticImmediate16(modRM)
                Else
                    ExecuteArithmeticImmediate32(modRM)
                End If
            Case &H83
                If prefixes.OperandSizeOverride Then
                    ExecuteArithmeticImmediate16SignExtended(modRM)
                Else
                    ExecuteArithmeticImmediate32SignExtended(modRM)
                End If
        End Select
    End Sub

    Private Sub ExecuteShiftRotate(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()
        Select Case modRM.Reg
            Case 0 : ExecuteROL(opcode)
            Case 1 : ExecuteROR(opcode)
            Case 2 : ExecuteRCL(opcode)
            Case 3 : ExecuteRCR(opcode)
            Case 4 : ExecuteSHL(opcode)
            Case 5 : ExecuteSHR(opcode)
            Case 7 : ExecuteSAR(opcode)
        End Select
    End Sub

    Private Sub ExecuteGroupF6F7(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()
        Select Case modRM.Reg
            Case 0, 1 : ExecuteTEST(opcode)
            Case 2 : ExecuteNOT(opcode)
            Case 3 : ExecuteNEG(opcode)
            Case 4 : ExecuteMUL(opcode)
            Case 5 : ExecuteIMUL(opcode)
            Case 6 : ExecuteDIV(opcode)
            Case 7 : ExecuteIDIV(opcode)
        End Select
    End Sub

    Private Sub ExecuteGroupFEFF(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()
        Select Case modRM.Reg
            Case 0 : ExecuteINC(opcode)
            Case 1 : ExecuteDEC(opcode)
            Case 2, 3 : ExecuteCALL(opcode)
            Case 4, 5 : ExecuteJMP(opcode)
            Case 6 : ExecutePUSH(opcode)
        End Select
    End Sub

    Private Sub ExecutePUSH_POP_SHLD_SHRD(secondOpcode As Byte)
        Select Case secondOpcode
            Case &HA0 : ExecutePUSHF()
            Case &HA1 : ExecutePOPF()
            Case &HA3 : ExecuteBT(secondOpcode)
            Case &HA4 : ExecuteSHLD(secondOpcode) ' Immediate count
            Case &HA5 : ExecuteSHLD(secondOpcode) ' CL count
            Case &HA8 : ExecutePUSH(secondOpcode)
            Case &HA9 : ExecutePOP(secondOpcode)
            Case &HAB : ExecuteBTS(secondOpcode)
            Case &HAC : ExecuteSHRD(secondOpcode) ' Immediate count
            Case &HAD : ExecuteSHRD(secondOpcode) ' CL count
            Case &HAF : ExecuteIMUL(secondOpcode)
        End Select
    End Sub

    Private Sub ExecuteCMPXCHG_XADD(secondOpcode As Byte)
        Select Case secondOpcode
            Case &HB0 : ExecuteCMPXCHG(secondOpcode)
            Case &HB1 : ExecuteCMPXCHG(secondOpcode)
            Case &HB3 : ExecuteBTR(secondOpcode)
            Case &HB6 : ExecuteMOVZX(secondOpcode)
            Case &HB7 : ExecuteMOVZX(secondOpcode)
            Case &HB8 : ExecutePOP(secondOpcode)
            Case &HBA : ExecuteBT_BTS_BTR_BTC()
            Case &HBB : ExecuteBTC(secondOpcode)
            Case &HBC : ExecuteBSF(secondOpcode)
            Case &HBD : ExecuteBSR()
            Case &HBE : ExecuteMOVSX(secondOpcode)
            Case &HBF : ExecuteMOVSX(secondOpcode)
        End Select
    End Sub

    Private Sub ExecuteBT_BTS_BTR_BTC()
        Dim Opcode As Byte = Mem.ReadByte(EIP)
        EIP += 1
        Select Case Opcode
            Case &HA3 ' BT r/m16/32, r16/32
                ExecuteBT(Opcode)

            Case &HAB ' BTS r/m16/32, r16/32
                ExecuteBTS(Opcode)

            Case &HB3 ' BTR r/m16/32, r16/32
                ExecuteBTR(Opcode)

            Case &HBB ' BTC r/m16/32, r16/32
                ExecuteBTC(Opcode)

            Case &HBA ' BT/BTS/BTR/BTC r/m16/32, imm8
                Dim modRM As ModRMByte = DecodeModRM()
                Select Case modRM.Reg
                    Case 4 ' BT r/m16/32, imm8
                        ExecuteBT(Opcode)
                    Case 5 ' BTS r/m16/32, imm8
                        ExecuteBTS(Opcode)
                    Case 6 ' BTR r/m16/32, imm8
                        ExecuteBTR(Opcode)
                    Case 7 ' BTC r/m16/32, imm8
                        ExecuteBTC(Opcode)
                    Case Else
                        LogError("Sub-opcode non valido per Bit Test immediato: " & modRM.Reg.ToString())
                End Select

            Case Else
                LogError("Opcode Bit Test non valido: " & Opcode.ToString("X2"))
        End Select
    End Sub

    Private Sub ExecuteFPUInstruction(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()
        Select Case opcode
            Case &HD8, &HDA, &HDC, &HDE ' FADD, FMUL, FCOM, FSUB, FDIV
                Select Case modRM.Reg
                    Case 0 : ExecuteFADD()
                    Case 1 : ExecuteFMUL()
                    Case 2 : ExecuteFCOM()
                    Case 4 : ExecuteFSUB()
                    Case 6 : ExecuteFDIV()
                End Select

            Case &HD9 ' FLD, FST, FSTP
                Select Case modRM.Reg
                    Case 0 : ExecuteFLD()
                    Case 2 : ExecuteFST()
                    Case 3 : ExecuteFSTP()
                End Select
                If modRM._Mod = 3 Then ' ST(0) operations
                    Select Case modRM.RM
                        Case 0 : ExecuteFLD() ' FLD ST(0)
                        Case 1 : ExecuteFXCH()
                        Case 4 : ExecuteFSQRT()
                        Case 5 : ExecuteFSCALE()
                    End Select
                End If

            Case &HDB, &HDD ' FCOMI, FCOMIP
                If modRM._Mod = 3 AndAlso modRM.Reg >= 6 Then
                    ExecuteFCOMIP() ' FCOMI/FCOMIP
                End If
        End Select
    End Sub

    Private Sub ExecuteExtendedInstructions()
        Dim secondOpcode As Byte = Mem.ReadByte(EIP)
        EIP += 1

        Select Case secondOpcode
            Case &H0
                ExecuteLIA()
        ' Istruzioni SIMD e SSE originali
            Case &H40 To &H4F : ExecuteCMOVcc(secondOpcode)

        ' Nuove istruzioni SSE/SIMD
            Case &H10, &H11 ' MOVUPS, MOVUPD
                If prefixes.OperandSizeOverride Then ExecuteMOVUPD() Else ExecuteMOVUPS()

            Case &H14, &H15 ' UNPCKLPS, UNPCKHPS, UNPCKLPD, UNPCKHPD
                If secondOpcode = &H14 Then
                    If prefixes.OperandSizeOverride Then ExecuteUNPCKLPD() Else ExecuteUNPCKLPS()
                Else
                    If prefixes.OperandSizeOverride Then ExecuteUNPCKHPD() Else ExecuteUNPCKHPS()
                End If

            Case &H17 : ExecuteEXTRACTPS()

            Case &H21 : ExecuteINSERTPS()

            Case &H28, &H29 ' MOVAPS, MOVAPD
                If prefixes.OperandSizeOverride Then ExecuteMOVAPD() Else ExecuteMOVAPS()

            Case &H2A ' CVTSI2 CVTSI2SD
                If prefixes.OperandSizeOverride Then ExecuteCVTSI2SD() Else ExecuteCVTSI2SS()

            Case &H40 : ExecutePMULLD()

            Case &H54 To &H57 ' ANDPS, ANDPD, ORPS, ORPD, XORPS, XORPD
                Select Case secondOpcode
                    Case &H54 : If prefixes.OperandSizeOverride Then ExecuteANDPD() Else ExecuteANDPS()
                    Case &H56 : If prefixes.OperandSizeOverride Then ExecuteORPD() Else ExecuteORPS()
                    Case &H57 : If prefixes.OperandSizeOverride Then ExecuteXORPD() Else ExecuteXORPS()
                End Select

            Case &H58 To &H5F ' Operazioni aritmetiche e di confronto
                Select Case secondOpcode
                    Case &H58 : If prefixes.OperandSizeOverride Then ExecuteADDPD() Else ExecuteADDPS()
                    Case &H59 : If prefixes.OperandSizeOverride Then ExecuteMULPD() Else ExecuteMULPS()
                    Case &H5A : If prefixes.OperandSizeOverride Then ExecuteCVTPD2PS() Else ExecuteCVTPS2PD()
                    Case &H5C : If prefixes.OperandSizeOverride Then ExecuteSUBPD() Else ExecuteSUBPS()
                    Case &H5D : If prefixes.OperandSizeOverride Then ExecuteMINPD() Else ExecuteMINPS()
                    Case &H5E : If prefixes.OperandSizeOverride Then ExecuteDIVPD() Else ExecuteDIVPS()
                    Case &H5F : If prefixes.OperandSizeOverride Then ExecuteMAXPD() Else ExecuteMAXPS()
                End Select

        ' Istruzioni SIMD e SSE originali
            Case &H80 To &H8F : ExecuteJcc(secondOpcode)
            Case &H90 To &H9F : ExecuteSETcc(secondOpcode)
            Case &HA0 To &HAF : ExecutePUSH_POP_SHLD_SHRD(secondOpcode)
            Case &HB0 To &HBF : ExecuteCMPXCHG_XADD(secondOpcode)

            Case &HC0 To &HC7
                Select Case secondOpcode
                    Case &HC0, &HC1
                        ExecuteXADD(secondOpcode)
                    Case &HC2
                        If prefixes.OperandSizeOverride Then ExecuteCMPPD() Else ExecuteCMPPS()
                    Case &HC6
                        If prefixes.OperandSizeOverride Then ExecuteSHUFPD() Else ExecuteSHUFPS()
                    Case Else
                        ExecuteBSWAP(secondOpcode)
                End Select

            Case &HC8 To &HCF : ExecuteBSWAP(secondOpcode)

                    Case &HF0 To &HFF ' Operazioni su interi SSE2
                        Select Case secondOpcode
                            Case &HFA : ExecutePSUBD()
                            Case &HFE : ExecutePADDD()
                                ' Altri casi per operazioni su interi SSE2 se necessario
                        End Select

                    Case Else
                        LogError("Opcode esteso non supportato: 0F " & secondOpcode.ToString("X2"))
                End Select

    End Sub


    Private Sub ExecuteAAA()
        If ((AL And &HF) > 9) OrElse (AF = 1) Then
            AL = (AL + 6) And &HF
            AH = (AH + 1) And &HFF
            AuxiliaryCarryFlag = True
            CarryFlag = True
        Else
            AuxiliaryCarryFlag = False
            CarryFlag = False
        End If
        AL = AL And &HF
    End Sub

    Private Sub ExecuteAAD()
        Dim temp As Byte = AL
        AL = (AH * 10 + temp) And &HFF
        AH = 0
        SetFlagsAfterOperation(AL, GetSizeMask(AL))
    End Sub

    Private Sub ExecuteAAM()
        Dim temp As Byte = AL
        AH = (temp \ 10) And &HFF
        AL = (temp Mod 10) And &HFF
        SetFlagsAfterOperation(AL, GetSizeMask(AL))
    End Sub

    Private Sub ExecuteAAS()
        If ((AL And &HF) > 9) OrElse (AF = 1) Then
            AL = (AL - 6) And &HF
            AH = (AH - 1) And &HFF
            AuxiliaryCarryFlag = 1
            CarryFlag = 1
        Else
            AuxiliaryCarryFlag = 0
            CarryFlag = 0
        End If
        AL = AL And &HF
    End Sub

    Private Sub ExecuteDAA()
        Dim oldAL As Byte = AL
        Dim oldCF As Boolean = CarryFlag

        If ((AL And &HF) > 9) OrElse AuxiliaryCarryFlag Then
            AL += 6
            CarryFlag = CarryFlag Or (AL < oldAL)
            AuxiliaryCarryFlag = True
        Else
            AuxiliaryCarryFlag = False
        End If

        If (oldAL > &H99) OrElse oldCF Then
            AL += &H60
            CarryFlag = True
        Else
            CarryFlag = False
        End If

        SetFlagsAfterOperation(AL, 8)
    End Sub

    Private Sub ExecuteDAS()
        Dim oldAL As Byte = AL
        Dim oldCF As Boolean = CarryFlag

        If ((AL And &HF) > 9) OrElse AuxiliaryCarryFlag Then
            AL -= 6
            CarryFlag = CarryFlag Or (AL > oldAL)
            AuxiliaryCarryFlag = True
        Else
            AuxiliaryCarryFlag = False
        End If

        If (oldAL > &H99) OrElse oldCF Then
            AL -= &H60
            CarryFlag = True
        Else
            CarryFlag = False
        End If

        SetFlagsAfterOperation(AL, 8)
    End Sub

    Private Sub ExecuteBOUND()
        Dim modRM As ModRMByte = DecodeModRM()

        If modRM._Mod = 3 Then
            LogError("Invalid addressing mode for BOUND instruction")
        End If

        Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
        Dim index As Integer
        Dim lowerBound As Integer
        Dim upperBound As Integer

        If prefixes.OperandSizeOverride Then
            ' 16-bit operands
            index = CInt(ReadRegister16(modRM.Reg))
            lowerBound = CInt(Mem.ReadWord(effectiveAddress))
            upperBound = CInt(Mem.ReadWord(effectiveAddress + 2))
        Else
            ' 32-bit operands
            index = CInt(ReadRegister32(modRM.Reg))
            lowerBound = CInt(Mem.ReadDWord(effectiveAddress))
            upperBound = CInt(Mem.ReadDWord(effectiveAddress + 4))
        End If

        If index < lowerBound OrElse index > upperBound Then
            ' Raise BOUND range exceeded exception (INT 5)
            ' RaiseException(5)
        End If

        ' BOUND does not affect flags
    End Sub

    Private Sub ExecuteLAHF()
        ' Load status flags into AH
        AH = CByte(GetEFLAGS() And &HFF)
    End Sub

    Private Sub ExecuteSAHF()
        ' Store AH into status flags
        Dim newFlags As UInteger = (GetEFLAGS() And &HFFFFFF00UI) Or AH
        SetEFLAGS(newFlags)
    End Sub

    Private Sub ExecuteArithmeticImmediate8(modRM As ModRMByte)
        Dim rm8 As Byte
        If modRM._Mod = 3 Then
            rm8 = ReadRegister8(modRM.RM)
        Else
            rm8 = Mem.ReadByte(DecodeEffectiveAddress(modRM))
        End If
        Dim imm8 As Byte = Mem.ReadByte(EIP)
        EIP += 1
        Dim result As UShort

        Select Case modRM.Reg
            Case 0 ' ADD
                result = CUShort(rm8) + CUShort(imm8)
                CarryFlag = result > &HFF
            Case 1 ' OR
                result = rm8 Or imm8
                CarryFlag = False
            Case 2 ' ADC
                result = CUShort(rm8) + CUShort(imm8) + CUShort(If(CarryFlag, 1, 0))
                CarryFlag = result > &HFF
            Case 3 ' SBB
                result = CUShort(rm8) - CUShort(imm8) - CUShort(If(CarryFlag, 1, 0))
                CarryFlag = result > &HFF
            Case 4 ' AND
                result = rm8 And imm8
                CarryFlag = False
            Case 5 ' SUB
                result = CUShort(rm8) - CUShort(imm8)
                CarryFlag = result > &HFF
            Case 6 ' XOR
                result = rm8 Xor imm8
                CarryFlag = False
            Case 7 ' CMP
                result = CUShort(rm8) - CUShort(imm8)
                CarryFlag = result > &HFF
                ' Non scrive il risultato per CMP
            Case Else
                LogError("Operazione aritmetica/logica non valida")
        End Select

        If modRM.Reg <> 7 Then ' Se non è CMP, scrivi il risultato
            If modRM._Mod = 3 Then
                WriteRegister8(modRM.RM, CByte(result And &HFF))
            Else
                Mem.WriteByte(DecodeEffectiveAddress(modRM), CByte(result And &HFF))
            End If
        End If

        SetFlagsAfterOperation(CByte(result And &HFF), 8)
    End Sub

    Private Sub ExecuteArithmeticImmediate16(modRM As ModRMByte)
        Dim rm16 As UShort
        If modRM._Mod = 3 Then
            rm16 = ReadRegister16(modRM.RM)
        Else
            rm16 = Mem.ReadWord(DecodeEffectiveAddress(modRM))
        End If
        Dim imm16 As UShort = Mem.ReadWord(EIP)
        EIP += 2
        Dim result As UInteger

        Select Case modRM.Reg
            Case 0 ' ADD
                result = CUInt(rm16) + CUInt(imm16)
                CarryFlag = result > &HFFFF
            Case 1 ' OR
                result = rm16 Or imm16
                CarryFlag = False
            Case 2 ' ADC
                result = CUInt(rm16) + CUInt(imm16) + CUInt(If(CarryFlag, 1, 0))
                CarryFlag = result > &HFFFF
            Case 3 ' SBB
                result = CUInt(rm16) - CUInt(imm16) - CUInt(If(CarryFlag, 1, 0))
                CarryFlag = result > &HFFFF
            Case 4 ' AND
                result = rm16 And imm16
                CarryFlag = False
            Case 5 ' SUB
                result = CUInt(rm16) - CUInt(imm16)
                CarryFlag = result > &HFFFF
            Case 6 ' XOR
                result = rm16 Xor imm16
                CarryFlag = False
            Case 7 ' CMP
                result = CUInt(rm16) - CUInt(imm16)
                CarryFlag = result > &HFFFF
                ' Non scrive il risultato per CMP
            Case Else
                LogError("Operazione aritmetica/logica non valida")
        End Select

        If modRM.Reg <> 7 Then ' Se non è CMP, scrivi il risultato
            If modRM._Mod = 3 Then
                WriteRegister16(modRM.RM, CUShort(result And &HFFFF))
            Else
                Mem.WriteWord(DecodeEffectiveAddress(modRM), CUShort(result And &HFFFF))
            End If
        End If

        SetFlagsAfterOperation(CUShort(result And &HFFFF), 16)
    End Sub

    Private Sub ExecuteArithmeticImmediate32(modRM As ModRMByte)
        Dim rm32 As UInteger
        If modRM._Mod = 3 Then
            rm32 = ReadRegister32(modRM.RM)
        Else
            rm32 = Mem.ReadDWord(DecodeEffectiveAddress(modRM))
        End If
        Dim imm32 As UInteger = Mem.ReadDWord(EIP)
        EIP += 4
        Dim result As ULong

        Select Case modRM.Reg
            Case 0 ' ADD
                result = CULng(rm32) + CULng(imm32)
                CarryFlag = result > UInteger.MaxValue
            Case 1 ' OR
                result = rm32 Or imm32
                CarryFlag = False
            Case 2 ' ADC
                result = CULng(rm32) + CULng(imm32) + CULng(If(CarryFlag, 1, 0))
                CarryFlag = result > UInteger.MaxValue
            Case 3 ' SBB
                result = CULng(rm32) - CULng(imm32) - CULng(If(CarryFlag, 1, 0))
                CarryFlag = result > UInteger.MaxValue
            Case 4 ' AND
                result = rm32 And imm32
                CarryFlag = False
            Case 5 ' SUB
                result = CULng(rm32) - CULng(imm32)
                CarryFlag = result > UInteger.MaxValue
            Case 6 ' XOR
                result = rm32 Xor imm32
                CarryFlag = False
            Case 7 ' CMP
                result = CULng(rm32) - CULng(imm32)
                CarryFlag = result > UInteger.MaxValue
                ' Non scrive il risultato per CMP
            Case Else
                LogError("Operazione aritmetica/logica non valida")
        End Select

        If modRM.Reg <> 7 Then ' Se non è CMP, scrivi il risultato
            If modRM._Mod = 3 Then
                WriteRegister32(modRM.RM, CUInt(result And &HFFFFFFFFUI))
            Else
                Mem.WriteDWord(DecodeEffectiveAddress(modRM), CUInt(result And &HFFFFFFFFUI))
            End If
        End If

        SetFlagsAfterOperation(CUInt(result And &HFFFFFFFFUI), 32)
    End Sub

    Private Sub ExecuteArithmeticImmediate16SignExtended(modRM As ModRMByte)
        Dim rm16 As UShort
        If modRM._Mod = 3 Then
            rm16 = ReadRegister16(modRM.RM)
        Else
            rm16 = Mem.ReadWord(DecodeEffectiveAddress(modRM))
        End If
        Dim imm8 As SByte = CSByte(Mem.ReadByte(EIP))
        EIP += 1
        Dim signExtendedImm As Short = imm8
        Dim result As UInteger

        Select Case modRM.Reg
            Case 0 ' ADD
                result = CUInt(rm16) + CUInt(signExtendedImm)
                CarryFlag = result > &HFFFF
            Case 1 ' OR
                result = rm16 Or CUShort(signExtendedImm)
                CarryFlag = False
            Case 2 ' ADC
                result = CUInt(rm16) + CUInt(signExtendedImm) + CUInt(If(CarryFlag, 1, 0))
                CarryFlag = result > &HFFFF
            Case 3 ' SBB
                result = CUInt(rm16) - CUInt(signExtendedImm) - CUInt(If(CarryFlag, 1, 0))
                CarryFlag = result > &HFFFF
            Case 4 ' AND
                result = rm16 And CUShort(signExtendedImm)
                CarryFlag = False
            Case 5 ' SUB
                result = CUInt(rm16) - CUInt(signExtendedImm)
                CarryFlag = result > &HFFFF
            Case 6 ' XOR
                result = rm16 Xor CUShort(signExtendedImm)
                CarryFlag = False
            Case 7 ' CMP
                result = CUInt(rm16) - CUInt(signExtendedImm)
                CarryFlag = result > &HFFFF
                ' Non scrive il risultato per CMP
            Case Else
                LogError("Operazione aritmetica/logica non valida")
        End Select

        If modRM.Reg <> 7 Then ' Se non è CMP, scrivi il risultato
            If modRM._Mod = 3 Then
                WriteRegister16(modRM.RM, CUShort(result And &HFFFF))
            Else
                Mem.WriteWord(DecodeEffectiveAddress(modRM), CUShort(result And &HFFFF))
            End If
        End If

        SetFlagsAfterOperation(CUShort(result And &HFFFF), 16)
    End Sub

    Private Sub ExecuteArithmeticImmediate32SignExtended(modRM As ModRMByte)
        Dim rm32 As UInteger
        If modRM._Mod = 3 Then
            rm32 = ReadRegister32(modRM.RM)
        Else
            rm32 = Mem.ReadDWord(DecodeEffectiveAddress(modRM))
        End If
        Dim imm8 As SByte = CSByte(Mem.ReadByte(EIP))
        EIP += 1
        Dim signExtendedImm As Integer = imm8
        Dim result As ULong

        Select Case modRM.Reg
            Case 0 ' ADD
                result = CULng(rm32) + CULng(signExtendedImm)
                CarryFlag = result > UInteger.MaxValue
            Case 1 ' OR
                result = rm32 Or CUInt(signExtendedImm)
                CarryFlag = False
            Case 2 ' ADC
                result = CULng(rm32) + CULng(signExtendedImm) + CULng(If(CarryFlag, 1, 0))
                CarryFlag = result > UInteger.MaxValue
            Case 3 ' SBB
                result = CULng(rm32) - CULng(signExtendedImm) - CULng(If(CarryFlag, 1, 0))
                CarryFlag = result > UInteger.MaxValue
            Case 4 ' AND
                result = rm32 And CUInt(signExtendedImm)
                CarryFlag = False
            Case 5 ' SUB
                result = CULng(rm32) - CULng(signExtendedImm)
                CarryFlag = result > UInteger.MaxValue
            Case 6 ' XOR
                result = rm32 Xor CUInt(signExtendedImm)
                CarryFlag = False
            Case 7 ' CMP
                result = CULng(rm32) - CULng(signExtendedImm)
                CarryFlag = result > UInteger.MaxValue
                ' Non scrive il risultato per CMP
            Case Else
                LogError("Operazione aritmetica/logica non valida")
        End Select

        If modRM.Reg <> 7 Then ' Se non è CMP, scrivi il risultato
            If modRM._Mod = 3 Then
                WriteRegister32(modRM.RM, CUInt(result And &HFFFFFFFFUI))
            Else
                Mem.WriteDWord(DecodeEffectiveAddress(modRM), CUInt(result And &HFFFFFFFFUI))
            End If
        End If

        SetFlagsAfterOperation(CUInt(result And &HFFFFFFFFUI), 32)
    End Sub

    Private Sub ExecuteADC(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()

        Select Case opcode
            Case &H10 ' ADC r/m8, r8
                Dim rm8 As Byte = If(modRM._Mod = 3, ReadRegister8(modRM.RM), Mem.ReadByte(DecodeEffectiveAddress(modRM)))
                Dim r8 As Byte = ReadRegister8(modRM.Reg)
                Dim result As UShort = CUShort(rm8) + CUShort(r8) + CUShort(If(CarryFlag, 1, 0))
                CarryFlag = result > &HFF
                If modRM._Mod = 3 Then
                    WriteRegister8(modRM.RM, CByte(result And &HFF))
                Else
                    Mem.WriteByte(DecodeEffectiveAddress(modRM), CByte(result And &HFF))
                End If
                SetFlagsAfterOperation(CByte(result And &HFF), 8)

            Case &H11 ' ADC r/m16, r16 or ADC r/m32, r32
                If prefixes.OperandSizeOverride Then
                    Dim rm16 As UShort = If(modRM._Mod = 3, ReadRegister16(modRM.RM), Mem.ReadWord(DecodeEffectiveAddress(modRM)))
                    Dim r16 As UShort = ReadRegister16(modRM.Reg)
                    Dim result As UInteger = CUInt(rm16) + CUInt(r16) + CUInt(If(CarryFlag, 1, 0))
                    CarryFlag = result > &HFFFF
                    If modRM._Mod = 3 Then
                        WriteRegister16(modRM.RM, CUShort(result And &HFFFF))
                    Else
                        Mem.WriteWord(DecodeEffectiveAddress(modRM), CUShort(result And &HFFFF))
                    End If
                    SetFlagsAfterOperation(CUShort(result And &HFFFF), 16)
                Else
                    Dim rm32 As UInteger = If(modRM._Mod = 3, ReadRegister32(modRM.RM), Mem.ReadDWord(DecodeEffectiveAddress(modRM)))
                    Dim r32 As UInteger = ReadRegister32(modRM.Reg)
                    Dim result As ULong = CULng(rm32) + CULng(r32) + CULng(If(CarryFlag, 1, 0))
                    CarryFlag = result > UInteger.MaxValue
                    If modRM._Mod = 3 Then
                        WriteRegister32(modRM.RM, CUInt(result And &HFFFFFFFFUI))
                    Else
                        Mem.WriteDWord(DecodeEffectiveAddress(modRM), CUInt(result And &HFFFFFFFFUI))
                    End If
                    SetFlagsAfterOperation(CUInt(result And &HFFFFFFFFUI), 32)
                End If

            Case &H12 ' ADC r8, r/m8
                Dim r8 As Byte = ReadRegister8(modRM.Reg)
                Dim rm8 As Byte = If(modRM._Mod = 3, ReadRegister8(modRM.RM), Mem.ReadByte(DecodeEffectiveAddress(modRM)))
                Dim result As UShort = CUShort(r8) + CUShort(rm8) + CUShort(If(CarryFlag, 1, 0))
                CarryFlag = result > &HFF
                WriteRegister8(modRM.Reg, CByte(result And &HFF))
                SetFlagsAfterOperation(CByte(result And &HFF), 8)

            Case &H13 ' ADC r16, r/m16 or ADC r32, r/m32
                If prefixes.OperandSizeOverride Then
                    Dim r16 As UShort = ReadRegister16(modRM.Reg)
                    Dim rm16 As UShort = If(modRM._Mod = 3, ReadRegister16(modRM.RM), Mem.ReadWord(DecodeEffectiveAddress(modRM)))
                    Dim result As UInteger = CUInt(r16) + CUInt(rm16) + CUInt(If(CarryFlag, 1, 0))
                    CarryFlag = result > &HFFFF
                    WriteRegister16(modRM.Reg, CUShort(result And &HFFFF))
                    SetFlagsAfterOperation(CUShort(result And &HFFFF), 16)
                Else
                    Dim r32 As UInteger = ReadRegister32(modRM.Reg)
                    Dim rm32 As UInteger = If(modRM._Mod = 3, ReadRegister32(modRM.RM), Mem.ReadDWord(DecodeEffectiveAddress(modRM)))
                    Dim result As ULong = CULng(r32) + CULng(rm32) + CULng(If(CarryFlag, 1, 0))
                    CarryFlag = result > UInteger.MaxValue
                    WriteRegister32(modRM.Reg, CUInt(result And &HFFFFFFFFUI))
                    SetFlagsAfterOperation(CUInt(result And &HFFFFFFFFUI), 32)
                End If

            Case &H14 ' ADC AL, imm8
                Dim imm8 As Byte = Mem.ReadByte(EIP)
                EIP += 1
                Dim result As UShort = CUShort(AL) + CUShort(imm8) + CUShort(If(CarryFlag, 1, 0))
                CarryFlag = result > &HFF
                AL = CByte(result And &HFF)
                SetFlagsAfterOperation(AL, 8)

            Case &H15 ' ADC AX, imm16 or ADC EAX, imm32
                If prefixes.OperandSizeOverride Then
                    Dim imm16 As UShort = Mem.ReadWord(EIP)
                    EIP += 2
                    Dim result As UInteger = CUInt(AX) + CUInt(imm16) + CUInt(If(CarryFlag, 1, 0))
                    CarryFlag = result > &HFFFF
                    AX = CUShort(result And &HFFFF)
                    SetFlagsAfterOperation(AX, 16)
                Else
                    Dim imm32 As UInteger = Mem.ReadDWord(EIP)
                    EIP += 4
                    Dim result As ULong = CULng(EAX) + CULng(imm32) + CULng(If(CarryFlag, 1, 0))
                    CarryFlag = result > UInteger.MaxValue
                    EAX = CUInt(result And &HFFFFFFFFUI)
                    SetFlagsAfterOperation(EAX, 32)
                End If

            Case Else
                LogError("Opcode ADC non supportato: " & opcode.ToString("X2"))
        End Select
    End Sub

    Private Sub ExecuteADD(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()

        Select Case opcode
            Case &H0 ' ADD r/m8, r8
                Dim rm8 As Byte = If(modRM._Mod = 3, ReadRegister8(modRM.RM), Mem.ReadByte(DecodeEffectiveAddress(modRM)))
                Dim r8 As Byte = ReadRegister8(modRM.Reg)
                Dim result As UShort = CUShort(rm8) + CUShort(r8)
                CarryFlag = result > &HFF
                If modRM._Mod = 3 Then
                    WriteRegister8(modRM.RM, CByte(result And &HFF))
                Else
                    Mem.WriteByte(DecodeEffectiveAddress(modRM), CByte(result And &HFF))
                End If
                SetFlagsAfterOperation(CByte(result And &HFF), 8)

            Case &H1 ' ADD r/m16, r16 or ADD r/m32, r32
                If prefixes.OperandSizeOverride Then
                    Dim rm16 As UShort = If(modRM._Mod = 3, ReadRegister16(modRM.RM), Mem.ReadWord(DecodeEffectiveAddress(modRM)))
                    Dim r16 As UShort = ReadRegister16(modRM.Reg)
                    Dim result As UInteger = CUInt(rm16) + CUInt(r16)
                    CarryFlag = result > &HFFFF
                    If modRM._Mod = 3 Then
                        WriteRegister16(modRM.RM, CUShort(result And &HFFFF))
                    Else
                        Mem.WriteWord(DecodeEffectiveAddress(modRM), CUShort(result And &HFFFF))
                    End If
                    SetFlagsAfterOperation(CUShort(result And &HFFFF), 16)
                Else
                    Dim rm32 As UInteger = If(modRM._Mod = 3, ReadRegister32(modRM.RM), Mem.ReadDWord(DecodeEffectiveAddress(modRM)))
                    Dim r32 As UInteger = ReadRegister32(modRM.Reg)
                    Dim result As ULong = CULng(rm32) + CULng(r32)
                    CarryFlag = result > UInteger.MaxValue
                    If modRM._Mod = 3 Then
                        WriteRegister32(modRM.RM, CUInt(result And &HFFFFFFFFUI))
                    Else
                        Mem.WriteDWord(DecodeEffectiveAddress(modRM), CUInt(result And &HFFFFFFFFUI))
                    End If
                    SetFlagsAfterOperation(CUInt(result And &HFFFFFFFFUI), 32)
                End If

            Case &H2 ' ADD r8, r/m8
                Dim r8 As Byte = ReadRegister8(modRM.Reg)
                Dim rm8 As Byte = If(modRM._Mod = 3, ReadRegister8(modRM.RM), Mem.ReadByte(DecodeEffectiveAddress(modRM)))
                Dim result As UShort = CUShort(r8) + CUShort(rm8)
                CarryFlag = result > &HFF
                WriteRegister8(modRM.Reg, CByte(result And &HFF))
                SetFlagsAfterOperation(CByte(result And &HFF), 8)

            Case &H3 ' ADD r16, r/m16 or ADD r32, r/m32
                If prefixes.OperandSizeOverride Then
                    Dim r16 As UShort = ReadRegister16(modRM.Reg)
                    Dim rm16 As UShort = If(modRM._Mod = 3, ReadRegister16(modRM.RM), Mem.ReadWord(DecodeEffectiveAddress(modRM)))
                    Dim result As UInteger = CUInt(r16) + CUInt(rm16)
                    CarryFlag = result > &HFFFF
                    WriteRegister16(modRM.Reg, CUShort(result And &HFFFF))
                    SetFlagsAfterOperation(CUShort(result And &HFFFF), 16)
                Else
                    Dim r32 As UInteger = ReadRegister32(modRM.Reg)
                    Dim rm32 As UInteger = If(modRM._Mod = 3, ReadRegister32(modRM.RM), Mem.ReadDWord(DecodeEffectiveAddress(modRM)))
                    Dim result As ULong = CULng(r32) + CULng(rm32)
                    CarryFlag = result > UInteger.MaxValue
                    WriteRegister32(modRM.Reg, CUInt(result And &HFFFFFFFFUI))
                    SetFlagsAfterOperation(CUInt(result And &HFFFFFFFFUI), 32)
                End If

            Case &H4 ' ADD AL, imm8
                Dim imm8 As Byte = Mem.ReadByte(EIP)
                EIP += 1
                Dim result As UShort = CUShort(AL) + CUShort(imm8)
                CarryFlag = result > &HFF
                AL = CByte(result And &HFF)
                SetFlagsAfterOperation(AL, 8)

            Case &H5 ' ADD AX, imm16 or ADD EAX, imm32
                If prefixes.OperandSizeOverride Then
                    Dim imm16 As UShort = Mem.ReadWord(EIP)
                    EIP += 2
                    Dim result As UInteger = CUInt(AX) + CUInt(imm16)
                    CarryFlag = result > &HFFFF
                    AX = CUShort(result And &HFFFF)
                    SetFlagsAfterOperation(AX, 16)
                Else
                    Dim imm32 As UInteger = Mem.ReadDWord(EIP)
                    EIP += 4
                    Dim result As ULong = CULng(EAX) + CULng(imm32)
                    CarryFlag = result > UInteger.MaxValue
                    EAX = CUInt(result And &HFFFFFFFFUI)
                    SetFlagsAfterOperation(EAX, 32)
                End If

            Case Else
                LogError("Opcode ADD non supportato: " & opcode.ToString("X2"))
        End Select
    End Sub

    Private Sub ExecuteAND(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()

        Select Case opcode
            Case &H20 ' AND r/m8, r8
                Dim rm8 As Byte = If(modRM._Mod = 3, ReadRegister8(modRM.RM), Mem.ReadByte(DecodeEffectiveAddress(modRM)))
                Dim r8 As Byte = ReadRegister8(modRM.Reg)
                Dim result As Byte = CByte(rm8 And r8)
                If modRM._Mod = 3 Then
                    WriteRegister8(modRM.RM, result)
                Else
                    Mem.WriteByte(DecodeEffectiveAddress(modRM), result)
                End If
                SetFlagsAfterLogicalOperation(result, 8)

            Case &H21 ' AND r/m16, r16 or AND r/m32, r32
                If prefixes.OperandSizeOverride Then
                    Dim rm16 As UShort = If(modRM._Mod = 3, ReadRegister16(modRM.RM), Mem.ReadWord(DecodeEffectiveAddress(modRM)))
                    Dim r16 As UShort = ReadRegister16(modRM.Reg)
                    Dim result As UShort = CUShort(rm16 And r16)
                    If modRM._Mod = 3 Then
                        WriteRegister16(modRM.RM, result)
                    Else
                        Mem.WriteWord(DecodeEffectiveAddress(modRM), result)
                    End If
                    SetFlagsAfterLogicalOperation(result, 16)
                Else
                    Dim rm32 As UInteger = If(modRM._Mod = 3, ReadRegister32(modRM.RM), Mem.ReadDWord(DecodeEffectiveAddress(modRM)))
                    Dim r32 As UInteger = ReadRegister32(modRM.Reg)
                    Dim result As UInteger = rm32 And r32
                    If modRM._Mod = 3 Then
                        WriteRegister32(modRM.RM, result)
                    Else
                        Mem.WriteDWord(DecodeEffectiveAddress(modRM), result)
                    End If
                    SetFlagsAfterLogicalOperation(result, 32)
                End If

            Case &H22 ' AND r8, r/m8
                Dim r8 As Byte = ReadRegister8(modRM.Reg)
                Dim rm8 As Byte = If(modRM._Mod = 3, ReadRegister8(modRM.RM), Mem.ReadByte(DecodeEffectiveAddress(modRM)))
                Dim result As Byte = CByte(r8 And rm8)
                WriteRegister8(modRM.Reg, result)
                SetFlagsAfterLogicalOperation(result, 8)

            Case &H23 ' AND r16, r/m16 or AND r32, r/m32
                If prefixes.OperandSizeOverride Then
                    Dim r16 As UShort = ReadRegister16(modRM.Reg)
                    Dim rm16 As UShort = If(modRM._Mod = 3, ReadRegister16(modRM.RM), Mem.ReadWord(DecodeEffectiveAddress(modRM)))
                    Dim result As UShort = CUShort(r16 And rm16)
                    WriteRegister16(modRM.Reg, result)
                    SetFlagsAfterLogicalOperation(result, 16)
                Else
                    Dim r32 As UInteger = ReadRegister32(modRM.Reg)
                    Dim rm32 As UInteger = If(modRM._Mod = 3, ReadRegister32(modRM.RM), Mem.ReadDWord(DecodeEffectiveAddress(modRM)))
                    Dim result As UInteger = r32 And rm32
                    WriteRegister32(modRM.Reg, result)
                    SetFlagsAfterLogicalOperation(result, 32)
                End If

            Case &H24 ' AND AL, imm8
                Dim imm8 As Byte = Mem.ReadByte(EIP)
                EIP += 1
                Dim result As Byte = CByte(AL And imm8)
                AL = result
                SetFlagsAfterLogicalOperation(result, 8)

            Case &H25 ' AND AX, imm16 or AND EAX, imm32
                If prefixes.OperandSizeOverride Then
                    Dim imm16 As UShort = Mem.ReadWord(EIP)
                    EIP += 2
                    Dim result As UShort = CUShort(AX And imm16)
                    AX = result
                    SetFlagsAfterLogicalOperation(result, 16)
                Else
                    Dim imm32 As UInteger = Mem.ReadDWord(EIP)
                    EIP += 4
                    Dim result As UInteger = EAX And imm32
                    EAX = result
                    SetFlagsAfterLogicalOperation(result, 32)
                End If

            Case Else
                LogError("Opcode AND non supportato: " & opcode.ToString("X2"))
        End Select
    End Sub

    Private Sub ExecuteLEA()
        Dim modRM As ModRMByte = DecodeModRM()

        If modRM._Mod = 3 Then
            LogError("Invalid addressing mode for LEA instruction")
        End If

        Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)

        If prefixes.OperandSizeOverride Then
            WriteRegister16(modRM.Reg, CUShort(effectiveAddress And &HFFFF))
        Else
            WriteRegister32(modRM.Reg, effectiveAddress)
        End If
    End Sub

    Private Sub ExecuteBSWAP(opcode As Byte)
        Dim reg As Byte = CByte(opcode And &H7)
        Dim value As UInteger = ReadRegister32(reg)
        Dim swapped As UInteger = ((value And &HFF) << 24) Or
                                   ((value And &HFF00) << 8) Or
                                   ((value And &HFF0000) >> 8) Or
                                   ((value And &HFF000000) >> 24)
        WriteRegister32(reg, swapped)
    End Sub

    Private Sub ExecuteSCAS(opcode As Byte)
        Dim size As Integer = If(opcode = &HAE, 8, If(prefixes.OperandSizeOverride, 16, 32))
        Dim value As UInteger
        Dim accumulator As UInteger

        Select Case size
            Case 8
                value = Mem.ReadByte(EDI)
                accumulator = AL
            Case 16
                value = Mem.ReadWord(EDI)
                accumulator = AX
            Case 32
                value = Mem.ReadDWord(EDI)
                accumulator = EAX
        End Select

        Dim result As UInteger = accumulator - value
        SetFlagsAfterOperation(result, size)

        ' Aggiorna EDI/DI
        If DirectionFlag Then
            EDI -= CUInt(size \ 8)
        Else
            EDI += CUInt(size \ 8)
        End If

        ' Gestione del prefisso REP
        If prefixes.RepeatPrefix <> 0 Then
            ECX -= 1
            If ECX <> 0 AndAlso ((prefixes.RepeatPrefix = &HF2 AndAlso Not ZeroFlag) OrElse (prefixes.RepeatPrefix = &HF3 AndAlso ZeroFlag)) Then
                EIP -= 1 ' Ripeti l'istruzione
            End If
        End If
        ' Resetta il prefisso REP
        prefixes.RepeatPrefix = 0
    End Sub

    Private Sub ExecuteSTC()
        CarryFlag = True
    End Sub

    Private Sub ExecuteSTD()
        DirectionFlag = True
    End Sub

    Private Sub ExecuteSTI()
        InterruptFlag = True
    End Sub

    Private Sub ExecuteCBW()
        If prefixes.OperandSizeOverride Then
            ' CWDE
            EAX = CInt(CShort(AX))
        Else
            ' CBW
            AX = CShort(CByte(AL))
        End If
    End Sub

    Private Sub ExecuteCWDE()
        If prefixes.OperandSizeOverride Then
            ' CBW
            AX = CShort(CByte(AL))
        Else
            ' CWDE
            EAX = CInt(CShort(AX))
        End If
    End Sub

    Private Sub ExecuteCLC()
        CarryFlag = False
    End Sub

    Private Sub ExecuteCLi()
        InterruptFlag = False
    End Sub

    Private Sub ExecuteCLD()
        DirectionFlag = False
    End Sub

    Private Sub ExecuteCMC()
        CarryFlag = Not CarryFlag
    End Sub

    Private Sub ExecuteCDQ()
        If prefixes.OperandSizeOverride Then
            ' CWD (Convert Word to Doubleword)
            If (AX And &H8000) <> 0 Then
                ' Se il bit più significativo di AX è 1, estendi il segno
                DX = &HFFFF
            Else
                ' Altrimenti, riempi con zeri
                DX = 0
            End If
        Else
            ' CDQ (Convert Doubleword to Quadword)
            If (EAX And &H80000000UI) <> 0 Then
                ' Se il bit più significativo di EAX è 1, estendi il segno
                EDX = &HFFFFFFFFUI
            Else
                ' Altrimenti, riempi con zeri
                EDX = 0
            End If
        End If
    End Sub

    Private Sub ExecuteLODS(opcode As Byte)
        Dim size As Integer = If(opcode = &HAC, 8, If(prefixes.OperandSizeOverride, 16, 32))

        Select Case size
            Case 8
                AL = Mem.ReadByte(ESI)
            Case 16
                AX = Mem.ReadWord(ESI)
            Case 32
                EAX = Mem.ReadDWord(ESI)
        End Select

        ' Aggiorna ESI/SI
        If DirectionFlag Then
            ESI -= CUInt(size \ 8)
        Else
            ESI += CUInt(size \ 8)
        End If

        ' Gestione del prefisso REP
        If prefixes.RepeatPrefix <> 0 Then
            ECX -= 1
            If ECX <> 0 Then
                EIP -= 1 ' Ripeti l'istruzione
            End If
        End If

        ' Resetta il prefisso REP
        prefixes.RepeatPrefix = 0

    End Sub

    Private Sub ExecuteSTOS(opcode As Byte)
        Dim size As Integer = If(opcode = &HAA, 8, If(prefixes.OperandSizeOverride, 16, 32))

        Select Case size
            Case 8
                Mem.WriteByte(EDI, AL)
            Case 16
                Mem.WriteWord(EDI, AX)
            Case 32
                Mem.WriteDWord(EDI, EAX)
        End Select

        ' Aggiorna EDI/DI
        If DirectionFlag Then
            EDI -= CUInt(size \ 8)
        Else
            EDI += CUInt(size \ 8)
        End If

        ' Gestione del prefisso REP
        If prefixes.RepeatPrefix <> 0 Then
            ECX -= 1
            If ECX <> 0 Then
                EIP -= 1 ' Ripeti l'istruzione
            End If
        End If

        ' Resetta il prefisso REP
        prefixes.RepeatPrefix = 0

    End Sub

    Private Sub ExecutePUSH(opcode As Byte)
        Dim size As Integer = If(prefixes.OperandSizeOverride, 16, 32)
        Dim value As UInteger

        Select Case opcode
            Case &H50 To &H57 ' PUSH r16/r32
                value = If(size = 16, ReadRegister16(opcode And 7), ReadRegister32(opcode And 7))

            Case &H6A ' PUSH imm8
                Dim imm8 As SByte = CSByte(Mem.ReadByte(EIP))
                EIP += 1
                value = CUInt(CInt(imm8)) ' Sign-extend to 32 bits

            Case &H68 ' PUSH imm16/imm32
                If size = 16 Then
                    value = Mem.ReadWord(EIP)
                    EIP += 2
                Else
                    value = Mem.ReadDWord(EIP)
                    EIP += 4
                End If

            Case &HFF ' PUSH r/m16/r/m32
                Dim modRM As ModRMByte = DecodeModRM()
                If modRM._Mod = 3 Then
                    value = If(size = 16, ReadRegister16(modRM.RM), ReadRegister32(modRM.RM))
                Else
                    Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                    value = If(size = 16, Mem.ReadWord(effectiveAddress), Mem.ReadDWord(effectiveAddress))
                End If

            Case Else
                LogError("Opcode PUSH non supportato: " & opcode.ToString("X2"))
        End Select

        ' Esegui il PUSH
        If size = 16 Then
            ESP -= 2
            Mem.WriteWord(ESP, CUShort(value))
        Else
            ESP -= 4
            Mem.WriteDWord(ESP, value)
        End If
    End Sub

    Private Sub ExecutePOP(opcode As Byte)
        Dim size As Integer = If(prefixes.OperandSizeOverride, 16, 32)
        Dim value As UInteger

        ' Esegui il POP
        If size = 16 Then
            value = Mem.ReadWord(ESP)
            ESP += 2
        Else
            value = Mem.ReadDWord(ESP)
            ESP += 4
        End If

        Select Case opcode
            Case &H58 To &H5F ' POP r16/r32
                If size = 16 Then
                    WriteRegister16(opcode And 7, CUShort(value))
                Else
                    WriteRegister32(opcode And 7, value)
                End If

            Case &H8F ' POP r/m16/r/m32
                Dim modRM As ModRMByte = DecodeModRM()
                If modRM._Mod = 3 Then
                    If size = 16 Then
                        WriteRegister16(modRM.RM, CUShort(value))
                    Else
                        WriteRegister32(modRM.RM, value)
                    End If
                Else
                    Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                    If size = 16 Then
                        Mem.WriteWord(effectiveAddress, CUShort(value))
                    Else
                        Mem.WriteDWord(effectiveAddress, value)
                    End If
                End If

            Case Else
                LogError("Opcode POP non supportato: " & opcode.ToString("X2"))
        End Select
    End Sub
    Private Sub ExecutePUSHA()
        ESP -= 32
        Mem.WriteDWord(ESP + 28, EAX)
        Mem.WriteDWord(ESP + 24, ECX)
        Mem.WriteDWord(ESP + 20, EDX)
        Mem.WriteDWord(ESP + 16, EBX)
        Mem.WriteDWord(ESP + 12, ESP)
        Mem.WriteDWord(ESP + 8, EBP)
        Mem.WriteDWord(ESP + 4, ESI)
        Mem.WriteDWord(ESP, EDI)
    End Sub

    Private Sub ExecutePOPA()
        EDI = Mem.ReadDWord(ESP)
        ESI = Mem.ReadDWord(ESP + 4)
        EBP = Mem.ReadDWord(ESP + 8)
        ' Skip ESP
        EBX = Mem.ReadDWord(ESP + 16)
        EDX = Mem.ReadDWord(ESP + 20)
        ECX = Mem.ReadDWord(ESP + 24)
        EAX = Mem.ReadDWord(ESP + 28)
        ESP += 32
    End Sub

    Private Sub ExecutePUSHF()
        Dim flags As UInteger = GetEFLAGS()
        ESP -= 4
        Mem.WriteDWord(ESP, flags)
    End Sub

    Private Sub ExecutePOPF()
        Dim flags As UInteger
        flags = Mem.ReadDWord(ESP)
        ESP += 4
        SetEFLAGS(flags)
    End Sub

    Private Sub ExecuteXLAT()
        Dim offset As UInteger = CUInt(BX) + CUInt(AL)
        AL = Mem.ReadByte(offset)
    End Sub

    Private Sub ExecuteJcc(opcode As Byte)
        Dim condition As Boolean = False
        Dim offset As Integer

        ' Determina la condizione in base all'opcode
        Select Case opcode
            Case &H70, &H80 : condition = OverflowFlag ' JO
            Case &H71, &H81 : condition = Not OverflowFlag ' JNO
            Case &H72, &H82 : condition = CarryFlag ' JB/JNAE/JC
            Case &H73, &H83 : condition = Not CarryFlag ' JAE/JNB/JNC
            Case &H74, &H84 : condition = ZeroFlag ' JE/JZ
            Case &H75, &H85 : condition = Not ZeroFlag ' JNE/JNZ
            Case &H76, &H86 : condition = CarryFlag Or ZeroFlag ' JBE/JNA
            Case &H77, &H87 : condition = Not (CarryFlag Or ZeroFlag) ' JA/JNBE
            Case &H78, &H88 : condition = SignFlag ' JS
            Case &H79, &H89 : condition = Not SignFlag ' JNS
            Case &H7A, &H8A : condition = ParityFlag ' JP/JPE
            Case &H7B, &H8B : condition = Not ParityFlag ' JNP/JPO
            Case &H7C, &H8C : condition = SignFlag <> OverflowFlag ' JL/JNGE
            Case &H7D, &H8D : condition = SignFlag = OverflowFlag ' JGE/JNL
            Case &H7E, &H8E : condition = ZeroFlag Or (SignFlag <> OverflowFlag) ' JLE/JNG
            Case &H7F, &H8F : condition = Not ZeroFlag And (SignFlag = OverflowFlag) ' JG/JNLE
            Case &HE3 : condition = (ECX = 0) ' JECXZ/JCXZ
            Case &HEB : condition = True ' JMP short
            Case &HE9 : condition = True ' JMP near
            Case Else
                LogError("Opcode di salto non supportato: " & opcode.ToString("X2"))
        End Select

        ' Determina la dimensione del salto
        Select Case opcode
            Case &H70 To &H7F, &HEB ' Short jump (8-bit offset)
                offset = CSByte(Mem.ReadByte(EIP))
                EIP += 1
            Case &H80 To &H8F, &HE9 ' Near jump (16/32-bit offset)
                If prefixes.OperandSizeOverride Then
                    offset = CShort(Mem.ReadWord(EIP))
                    EIP += 2
                Else
                    offset = CInt(Mem.ReadDWord(EIP))
                    EIP += 4
                End If
            Case &HE3 ' JECXZ/JCXZ (always 8-bit offset)
                offset = CSByte(Mem.ReadByte(EIP))
                EIP += 1
        End Select

        ' Esegui il salto se la condizione è vera
        If condition Then
            EIP += CUInt(offset)
        End If
    End Sub

    Private Sub ExecuteSETcc(secondOpcode As Byte)
        Dim condition As Boolean = False
        Select Case secondOpcode
            Case &H90 : condition = OverflowFlag ' SETO
            Case &H91 : condition = Not OverflowFlag ' SETNO
            Case &H92 : condition = CarryFlag ' SETB/SETNAE/SETC
            Case &H93 : condition = Not CarryFlag ' SETAE/SETNB/SETNC
            Case &H94 : condition = ZeroFlag ' SETE/SETZ
            Case &H95 : condition = Not ZeroFlag ' SETNE/SETNZ
            Case &H96 : condition = CarryFlag Or ZeroFlag ' SETBE/SETNA
            Case &H97 : condition = Not (CarryFlag Or ZeroFlag) ' SETA/SETNBE
            Case &H98 : condition = SignFlag ' SETS
            Case &H99 : condition = Not SignFlag ' SETNS
            Case &H9A : condition = ParityFlag ' SETP/SETPE
            Case &H9B : condition = Not ParityFlag ' SETNP/SETPO
            Case &H9C : condition = SignFlag <> OverflowFlag ' SETL/SETNGE
            Case &H9D : condition = SignFlag = OverflowFlag ' SETGE/SETNL
            Case &H9E : condition = ZeroFlag Or (SignFlag <> OverflowFlag) ' SETLE/SETNG
            Case &H9F : condition = Not ZeroFlag And (SignFlag = OverflowFlag) ' SETG/SETNLE
        End Select

        Dim modRM As ModRMByte = DecodeModRM()
        If modRM._Mod = 3 Then
            ' Registro
            If condition Then
                WriteRegister8(modRM.RM, 1)
            Else
                WriteRegister8(modRM.RM, 0)
            End If
        Else
            ' Memoria
            Dim address As UInteger = DecodeEffectiveAddress(modRM)
            If condition Then
                Mem.WriteByte(address, 1)
            Else
                Mem.WriteByte(address, 0)
            End If
        End If
    End Sub

    Private Sub ExecuteCMOVcc(secondOpcode As Byte)
        Dim condition As Boolean = False
        Select Case secondOpcode
            Case &H40 : condition = OverflowFlag ' CMOVO
            Case &H41 : condition = Not OverflowFlag ' CMOVNO
            Case &H42 : condition = CarryFlag ' CMOVB/CMOVC
            Case &H43 : condition = Not CarryFlag ' CMOVAE/CMOVNB
            Case &H44 : condition = ZeroFlag ' CMOVE/CMOVZ
            Case &H45 : condition = Not ZeroFlag ' CMOVNE/CMOVNZ
            Case &H46 : condition = CarryFlag Or ZeroFlag ' CMOVBE/CMOVNA
            Case &H47 : condition = Not (CarryFlag Or ZeroFlag) ' CMOVA/CMOVNBE
            Case &H48 : condition = SignFlag ' CMOVS
            Case &H49 : condition = Not SignFlag ' CMOVNS
            Case &H4A : condition = ParityFlag ' CMOVP/CMOVPE
            Case &H4B : condition = Not ParityFlag ' CMOVNP/CMOVPO
            Case &H4C : condition = SignFlag <> OverflowFlag ' CMOVL/CMOVNGE
            Case &H4D : condition = SignFlag = OverflowFlag ' CMOVGE/CMOVNL
            Case &H4E : condition = ZeroFlag Or (SignFlag <> OverflowFlag) ' CMOVLE/CMOVNG
            Case &H4F : condition = Not ZeroFlag And (SignFlag = OverflowFlag) ' CMOVG/CMOVNLE
        End Select
        If condition Then
            ExecuteMOV(secondOpcode - &H40)
        Else
            ' Non eseguire il MOV
            EIP += 2 ' Salta l'operando ModR/M
        End If
    End Sub

    Private Sub ExecuteXCHG(opcode As Byte)
        Select Case opcode
            Case &H90 To &H97 ' XCHG AX/EAX, r16/32
                Dim size As Integer = If(prefixes.OperandSizeOverride, 16, 32)
                Dim regIndex As Byte = CByte(opcode - &H90)
                Dim accumulator, regValue As UInteger

                If size = 16 Then
                    accumulator = AX
                    regValue = ReadRegister16(regIndex)
                    AX = CUShort(regValue)
                    WriteRegister16(regIndex, CUShort(accumulator))
                Else
                    accumulator = EAX
                    regValue = ReadRegister32(regIndex)
                    EAX = regValue
                    WriteRegister32(regIndex, accumulator)
                End If

            Case &H86 ' XCHG r/m8, r8
                Dim modRM As ModRMByte = DecodeModRM()
                Dim r8 As Byte = ReadRegister8(modRM.Reg)

                If modRM._Mod = 3 Then
                    Dim rm8 As Byte = ReadRegister8(modRM.RM)
                    WriteRegister8(modRM.Reg, rm8)
                    WriteRegister8(modRM.RM, r8)
                Else
                    Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                    Dim m8 As Byte = Mem.ReadByte(effectiveAddress)
                    WriteRegister8(modRM.Reg, m8)
                    Mem.WriteByte(effectiveAddress, r8)
                End If

            Case &H87 ' XCHG r/m16/32, r16/32
                Dim modRM As ModRMByte = DecodeModRM()
                Dim size As Integer = If(prefixes.OperandSizeOverride, 16, 32)

                If size = 16 Then
                    Dim r16 As UShort = ReadRegister16(modRM.Reg)
                    If modRM._Mod = 3 Then
                        Dim rm16 As UShort = ReadRegister16(modRM.RM)
                        WriteRegister16(modRM.Reg, rm16)
                        WriteRegister16(modRM.RM, r16)
                    Else
                        Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                        Dim m16 As UShort = Mem.ReadWord(effectiveAddress)
                        WriteRegister16(modRM.Reg, m16)
                        Mem.WriteWord(effectiveAddress, r16)
                    End If
                Else
                    Dim r32 As UInteger = ReadRegister32(modRM.Reg)
                    If modRM._Mod = 3 Then
                        Dim rm32 As UInteger = ReadRegister32(modRM.RM)
                        WriteRegister32(modRM.Reg, rm32)
                        WriteRegister32(modRM.RM, r32)
                    Else
                        Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                        Dim m32 As UInteger = Mem.ReadDWord(effectiveAddress)
                        WriteRegister32(modRM.Reg, m32)
                        Mem.WriteDWord(effectiveAddress, r32)
                    End If
                End If

            Case Else
                LogError("Opcode XCHG non valido: " & opcode.ToString("X2"))
        End Select
    End Sub

    Private Sub ExecuteMOV(opcode As Byte)
        Select Case opcode
            Case &H88 ' MOV r/m8, r8
                Dim modRM As ModRMByte = DecodeModRM()
                Dim r8 As Byte = ReadRegister8(modRM.Reg)
                If modRM._Mod = 3 Then
                    WriteRegister8(modRM.RM, r8)
                Else
                    Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                    Mem.WriteByte(effectiveAddress, r8)
                End If

            Case &H89 ' MOV r/m16/32, r16/32
                Dim modRM As ModRMByte = DecodeModRM()
                If prefixes.OperandSizeOverride Then
                    Dim r16 As UShort = ReadRegister16(modRM.Reg)
                    If modRM._Mod = 3 Then
                        WriteRegister16(modRM.RM, r16)
                    Else
                        Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                        Mem.WriteWord(effectiveAddress, r16)
                    End If
                Else
                    Dim r32 As UInteger = ReadRegister32(modRM.Reg)
                    If modRM._Mod = 3 Then
                        WriteRegister32(modRM.RM, r32)
                    Else
                        Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                        Mem.WriteDWord(effectiveAddress, r32)
                    End If
                End If

            Case &H8A ' MOV r8, r/m8
                Dim modRM As ModRMByte = DecodeModRM()
                Dim value As Byte
                If modRM._Mod = 3 Then
                    value = ReadRegister8(modRM.RM)
                Else
                    Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                    value = Mem.ReadByte(effectiveAddress)
                End If
                WriteRegister8(modRM.Reg, value)

            Case &H8B ' MOV r16/32, r/m16/32
                Dim modRM As ModRMByte = DecodeModRM()
                If prefixes.OperandSizeOverride Then
                    Dim value As UShort
                    If modRM._Mod = 3 Then
                        value = ReadRegister16(modRM.RM)
                    Else
                        Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                        value = Mem.ReadWord(effectiveAddress)
                    End If
                    WriteRegister16(modRM.Reg, value)
                Else
                    Dim value As UInteger
                    If modRM._Mod = 3 Then
                        value = ReadRegister32(modRM.RM)
                    Else
                        Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                        value = Mem.ReadDWord(effectiveAddress)
                    End If
                    WriteRegister32(modRM.Reg, value)
                End If

            Case &HA0 ' MOV AL, moffs8
                Dim address As UInteger = If(prefixes.AddressSizeOverride, Mem.ReadWord(EIP), Mem.ReadDWord(EIP))
                EIP += If(prefixes.AddressSizeOverride, 2UI, 4UI)
                AL = Mem.ReadByte(address)

            Case &HA1 ' MOV AX/EAX, moffs16/32
                Dim address As UInteger = If(prefixes.AddressSizeOverride, Mem.ReadWord(EIP), Mem.ReadDWord(EIP))
                EIP += If(prefixes.AddressSizeOverride, 2UI, 4UI)
                If prefixes.OperandSizeOverride Then
                    AX = Mem.ReadWord(address)
                Else
                    EAX = Mem.ReadDWord(address)
                End If

            Case &HA2 ' MOV moffs8, AL
                Dim address As UInteger = If(prefixes.AddressSizeOverride, Mem.ReadWord(EIP), Mem.ReadDWord(EIP))
                EIP += If(prefixes.AddressSizeOverride, 2UI, 4UI)
                Mem.WriteByte(address, AL)

            Case &HA3 ' MOV moffs16/32, AX/EAX
                Dim address As UInteger = If(prefixes.AddressSizeOverride, Mem.ReadWord(EIP), Mem.ReadDWord(EIP))
                EIP += If(prefixes.AddressSizeOverride, 2UI, 4UI)
                If prefixes.OperandSizeOverride Then
                    Mem.WriteWord(address, AX)
                Else
                    Mem.WriteDWord(address, EAX)
                End If

            Case &HB0 To &HB7 ' MOV r8, imm8
                Dim regIndex As Byte = CByte(opcode And &H7)
                Dim imm8 As Byte = Mem.ReadByte(EIP)
                EIP += 1
                WriteRegister8(regIndex, imm8)

            Case &HB8 To &HBF ' MOV r16/32, imm16/32
                Dim regIndex As Byte = CByte(opcode And &H7)
                If prefixes.OperandSizeOverride Then
                    Dim imm16 As UShort = Mem.ReadWord(EIP)
                    EIP += 2
                    WriteRegister16(regIndex, imm16)
                Else
                    Dim imm32 As UInteger = Mem.ReadDWord(EIP)
                    EIP += 4
                    WriteRegister32(regIndex, imm32)
                End If

            Case &HC6 ' MOV r/m8, imm8
                Dim modRM As ModRMByte = DecodeModRM()
                Dim imm8 As Byte = Mem.ReadByte(EIP)
                EIP += 1
                If modRM._Mod = 3 Then
                    WriteRegister8(modRM.RM, imm8)
                Else
                    Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                    Mem.WriteByte(effectiveAddress, imm8)
                End If

            Case &HC7 ' MOV r/m16/32, imm16/32
                Dim modRM As ModRMByte = DecodeModRM()
                If prefixes.OperandSizeOverride Then
                    Dim imm16 As UShort = Mem.ReadWord(EIP)
                    EIP += 2
                    If modRM._Mod = 3 Then
                        WriteRegister16(modRM.RM, imm16)
                    Else
                        Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                        Mem.WriteWord(effectiveAddress, imm16)
                    End If
                Else
                    Dim imm32 As UInteger = Mem.ReadDWord(EIP)
                    EIP += 4
                    If modRM._Mod = 3 Then
                        WriteRegister32(modRM.RM, imm32)
                    Else
                        Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                        Mem.WriteDWord(effectiveAddress, imm32)
                    End If
                End If

            Case Else
                LogError("Opcode MOV non valido: " & opcode.ToString("X2"))
        End Select
    End Sub

    Private Sub ExecuteMOVZX(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()

        Select Case opcode
            Case &HB6 ' MOVZX r16/32, r/m8
                Dim sourceValue As Byte
                If modRM._Mod = 3 Then
                    sourceValue = ReadRegister8(modRM.RM)
                Else
                    Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                    sourceValue = Mem.ReadByte(effectiveAddress)
                End If

                If prefixes.OperandSizeOverride Then
                    WriteRegister16(modRM.Reg, CUShort(sourceValue))
                Else
                    WriteRegister32(modRM.Reg, CUInt(sourceValue))
                End If

            Case &HB7 ' MOVZX r32, r/m16
                If Not prefixes.OperandSizeOverride Then
                    Dim sourceValue As UShort
                    If modRM._Mod = 3 Then
                        sourceValue = ReadRegister16(modRM.RM)
                    Else
                        Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                        sourceValue = Mem.ReadWord(effectiveAddress)
                    End If

                    WriteRegister32(modRM.Reg, CUInt(sourceValue))
                Else
                    LogError("Invalid operand size for MOVZX r32, r/m16")
                End If

            Case Else
                LogError("Opcode MOVZX non valido: " & opcode.ToString("X2"))
        End Select
    End Sub

    Private Sub ExecuteMOVSX(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()

        Select Case opcode
            Case &HBE ' MOVSX r16/32, r/m8
                Dim sourceValue As SByte
                If modRM._Mod = 3 Then
                    sourceValue = CSByte(ReadRegister8(modRM.RM))
                Else
                    Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                    sourceValue = CSByte(Mem.ReadByte(effectiveAddress))
                End If

                If prefixes.OperandSizeOverride Then
                    ' 16-bit destination
                    Dim extendedValue As Short = CShort(sourceValue)
                    WriteRegister16(modRM.Reg, CUShort(extendedValue))
                Else
                    ' 32-bit destination
                    Dim extendedValue As Integer = CInt(sourceValue)
                    WriteRegister32(modRM.Reg, CUInt(extendedValue))
                End If

            Case &HBF ' MOVSX r32, r/m16
                If Not prefixes.OperandSizeOverride Then
                    Dim sourceValue As Short
                    If modRM._Mod = 3 Then
                        sourceValue = CShort(ReadRegister16(modRM.RM))
                    Else
                        Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                        sourceValue = CShort(Mem.ReadWord(effectiveAddress))
                    End If

                    Dim extendedValue As Integer = CInt(sourceValue)
                    WriteRegister32(modRM.Reg, CUInt(extendedValue))
                Else
                    LogError("Invalid operand size for MOVSX r32, r/m16")
                End If

            Case Else
                LogError("Opcode MOVSX non valido: " & opcode.ToString("X2"))
        End Select
    End Sub

    Private Sub ExecuteCMP(opcode As Byte)
        Select Case opcode
            Case &H38 ' CMP r/m8, r8
                Dim modRM As ModRMByte = DecodeModRM()
                Dim r8 As Byte = ReadRegister8(modRM.Reg)
                Dim rm8 As Byte
                If modRM._Mod = 3 Then
                    rm8 = ReadRegister8(modRM.RM)
                Else
                    Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                    rm8 = Mem.ReadByte(effectiveAddress)
                End If
                Dim result As UShort = CUShort(rm8) - CUShort(r8)
                SetFlagsAfterOperation(CByte(result), 8)

            Case &H39 ' CMP r/m16/32, r16/32
                Dim modRM As ModRMByte = DecodeModRM()
                If prefixes.OperandSizeOverride Then
                    Dim r16 As UShort = ReadRegister16(modRM.Reg)
                    Dim rm16 As UShort
                    If modRM._Mod = 3 Then
                        rm16 = ReadRegister16(modRM.RM)
                    Else
                        Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                        rm16 = Mem.ReadWord(effectiveAddress)
                    End If
                    Dim result As UInteger = CUInt(rm16) - CUInt(r16)
                    SetFlagsAfterOperation(CUShort(result), 16)
                Else
                    Dim r32 As UInteger = ReadRegister32(modRM.Reg)
                    Dim rm32 As UInteger
                    If modRM._Mod = 3 Then
                        rm32 = ReadRegister32(modRM.RM)
                    Else
                        Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                        rm32 = Mem.ReadDWord(effectiveAddress)
                    End If
                    Dim result As ULong = CULng(rm32) - CULng(r32)
                    SetFlagsAfterOperation(CUInt(result), 32)
                End If

            Case &H3A ' CMP r8, r/m8
                Dim modRM As ModRMByte = DecodeModRM()
                Dim r8 As Byte = ReadRegister8(modRM.Reg)
                Dim rm8 As Byte
                If modRM._Mod = 3 Then
                    rm8 = ReadRegister8(modRM.RM)
                Else
                    Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                    rm8 = Mem.ReadByte(effectiveAddress)
                End If
                Dim result As UShort = CUShort(r8) - CUShort(rm8)
                SetFlagsAfterOperation(CByte(result), 8)

            Case &H3B ' CMP r16/32, r/m16/32
                Dim modRM As ModRMByte = DecodeModRM()
                If prefixes.OperandSizeOverride Then
                    Dim r16 As UShort = ReadRegister16(modRM.Reg)
                    Dim rm16 As UShort
                    If modRM._Mod = 3 Then
                        rm16 = ReadRegister16(modRM.RM)
                    Else
                        Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                        rm16 = Mem.ReadWord(effectiveAddress)
                    End If
                    Dim result As UInteger = CUInt(r16) - CUInt(rm16)
                    SetFlagsAfterOperation(CUShort(result), 16)
                Else
                    Dim r32 As UInteger = ReadRegister32(modRM.Reg)
                    Dim rm32 As UInteger
                    If modRM._Mod = 3 Then
                        rm32 = ReadRegister32(modRM.RM)
                    Else
                        Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                        rm32 = Mem.ReadDWord(effectiveAddress)
                    End If
                    Dim result As ULong = CULng(r32) - CULng(rm32)
                    SetFlagsAfterOperation(CUInt(result), 32)
                End If

            Case &H3C ' CMP AL, imm8
                Dim imm8 As Byte = Mem.ReadByte(EIP)
                EIP += 1
                Dim result As UShort = CUShort(AL) - CUShort(imm8)
                SetFlagsAfterOperation(CByte(result), 8)

            Case &H3D ' CMP AX/EAX, imm16/32
                If prefixes.OperandSizeOverride Then
                    Dim imm16 As UShort = Mem.ReadWord(EIP)
                    EIP += 2
                    Dim result As UInteger = CUInt(AX) - CUInt(imm16)
                    SetFlagsAfterOperation(CUShort(result), 16)
                Else
                    Dim imm32 As UInteger = Mem.ReadDWord(EIP)
                    EIP += 4
                    Dim result As ULong = CULng(EAX) - CULng(imm32)
                    SetFlagsAfterOperation(CUInt(result), 32)
                End If

            Case &H80, &H82 ' CMP r/m8, imm8
                Dim modRM As ModRMByte = DecodeModRM()
                Dim imm8 As Byte = Mem.ReadByte(EIP)
                EIP += 1
                Dim rm8 As Byte
                If modRM._Mod = 3 Then
                    rm8 = ReadRegister8(modRM.RM)
                Else
                    Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                    rm8 = Mem.ReadByte(effectiveAddress)
                End If
                Dim result As UShort = CUShort(rm8) - CUShort(imm8)
                SetFlagsAfterOperation(CByte(result), 8)

            Case &H81 ' CMP r/m16/32, imm16/32
                Dim modRM As ModRMByte = DecodeModRM()
                If prefixes.OperandSizeOverride Then
                    Dim imm16 As UShort = Mem.ReadWord(EIP)
                    EIP += 2
                    Dim rm16 As UShort
                    If modRM._Mod = 3 Then
                        rm16 = ReadRegister16(modRM.RM)
                    Else
                        Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                        rm16 = Mem.ReadWord(effectiveAddress)
                    End If
                    Dim result As UInteger = CUInt(rm16) - CUInt(imm16)
                    SetFlagsAfterOperation(CUShort(result), 16)
                Else
                    Dim imm32 As UInteger = Mem.ReadDWord(EIP)
                    EIP += 4
                    Dim rm32 As UInteger
                    If modRM._Mod = 3 Then
                        rm32 = ReadRegister32(modRM.RM)
                    Else
                        Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                        rm32 = Mem.ReadDWord(effectiveAddress)
                    End If
                    Dim result As ULong = CULng(rm32) - CULng(imm32)
                    SetFlagsAfterOperation(CUInt(result), 32)
                End If

            Case &H83 ' CMP r/m16/32, imm8 (sign-extended)
                Dim modRM As ModRMByte = DecodeModRM()
                Dim imm8 As SByte = CSByte(Mem.ReadByte(EIP))
                EIP += 1
                If prefixes.OperandSizeOverride Then
                    Dim rm16 As UShort
                    If modRM._Mod = 3 Then
                        rm16 = ReadRegister16(modRM.RM)
                    Else
                        Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                        rm16 = Mem.ReadWord(effectiveAddress)
                    End If
                    Dim result As Integer = CInt(rm16) - CInt(imm8)
                    SetFlagsAfterOperation(CUShort(result), 16)
                Else
                    Dim rm32 As UInteger
                    If modRM._Mod = 3 Then
                        rm32 = ReadRegister32(modRM.RM)
                    Else
                        Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                        rm32 = Mem.ReadDWord(effectiveAddress)
                    End If
                    Dim result As Long = CLng(rm32) - CLng(imm8)
                    SetFlagsAfterOperation(CUInt(result), 32)
                End If

            Case Else
                LogError("Opcode CMP non valido: " & opcode.ToString("X2"))
        End Select
    End Sub

    Private Sub ExecuteCMPS(opcode As Byte)
        Dim size As Integer = If(opcode = &HA6, 8, If(prefixes.OperandSizeOverride, 16, 32))
        Dim value1, value2 As UInteger
        Dim increment As Integer = size \ 8

        ' Leggi i valori dalla memoria
        Select Case size
            Case 8
                value1 = Mem.ReadByte(ESI)
                value2 = Mem.ReadByte(EDI)
            Case 16
                value1 = Mem.ReadWord(ESI)
                value2 = Mem.ReadWord(EDI)
            Case 32
                value1 = Mem.ReadDWord(ESI)
                value2 = Mem.ReadDWord(EDI)
        End Select

        ' Esegui la comparazione
        Dim result As UInteger = value1 - value2
        SetFlagsAfterOperation(result, size)

        ' Aggiorna ESI e EDI
        If DirectionFlag Then
            ESI -= CUInt(increment)
            EDI -= CUInt(increment)
        Else
            ESI += CUInt(increment)
            EDI += CUInt(increment)
        End If

        ' Gestione del prefisso REP
        If prefixes.RepeatPrefix <> 0 Then
            ECX -= 1
            If ECX <> 0 AndAlso ((prefixes.RepeatPrefix = &HF2 AndAlso Not ZeroFlag) OrElse (prefixes.RepeatPrefix = &HF3 AndAlso ZeroFlag)) Then
                EIP -= 1 ' Ripeti l'istruzione
            End If
        End If
    End Sub

    Private Sub ExecuteCMPXCHG(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()
        Dim size As Integer = If(opcode = &HB0, 8, If(prefixes.OperandSizeOverride, 16, 32))
        Dim accumulator, comparand, destination As UInteger

        ' Leggi l'accumulatore (AL/AX/EAX)
        Select Case size
            Case 8
                accumulator = AL
            Case 16
                accumulator = AX
            Case 32
                accumulator = EAX
        End Select

        ' Leggi il valore di comparazione dal registro
        Select Case size
            Case 8
                comparand = ReadRegister8(modRM.Reg)
            Case 16
                comparand = ReadRegister16(modRM.Reg)
            Case 32
                comparand = ReadRegister32(modRM.Reg)
        End Select

        ' Leggi il valore di destinazione
        If modRM._Mod = 3 Then
            ' Operazione registro-registro
            Select Case size
                Case 8
                    destination = ReadRegister8(modRM.RM)
                Case 16
                    destination = ReadRegister16(modRM.RM)
                Case 32
                    destination = ReadRegister32(modRM.RM)
            End Select
        Else
            ' Operazione memoria-registro
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            Select Case size
                Case 8
                    destination = Mem.ReadByte(effectiveAddress)
                Case 16
                    destination = Mem.ReadWord(effectiveAddress)
                Case 32
                    destination = Mem.ReadDWord(effectiveAddress)
            End Select
        End If

        ' Esegui l'operazione CMPXCHG
        If accumulator = destination Then
            ZeroFlag = True
            ' Scrivi il valore di comparazione nella destinazione
            If modRM._Mod = 3 Then
                Select Case size
                    Case 8
                        WriteRegister8(modRM.RM, CByte(comparand))
                    Case 16
                        WriteRegister16(modRM.RM, CUShort(comparand))
                    Case 32
                        WriteRegister32(modRM.RM, comparand)
                End Select
            Else
                Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                Select Case size
                    Case 8
                        Mem.WriteByte(effectiveAddress, CByte(comparand))
                    Case 16
                        Mem.WriteWord(effectiveAddress, CUShort(comparand))
                    Case 32
                        Mem.WriteDWord(effectiveAddress, comparand)
                End Select
            End If
        Else
            ZeroFlag = False
            ' Aggiorna l'accumulatore con il valore di destinazione
            Select Case size
                Case 8
                    AL = CByte(destination)
                Case 16
                    AX = CUShort(destination)
                Case 32
                    EAX = destination
            End Select
        End If

        ' Imposta i flag basati sulla comparazione
        SetFlagsAfterOperation(accumulator - destination, size)
    End Sub

    Private Sub ExecuteHLT()
        RaiseEvent Halt(Me)
    End Sub

    Private Sub ExecuteNOP()
        Thread.Sleep(1) ' Previene un loop troppo stretto
        ' NOP non fa nulla
    End Sub

    Private Sub ExecuteRET(opcode As Byte)
        Dim returnAddress As UInteger

        returnAddress = Mem.ReadDWord(ESP)
        ESP += 4

        EIP = returnAddress

        If opcode = &HC2 Then ' RET imm16
            Dim imm16 As UShort = Mem.ReadWord(EIP)
            EIP += 2
            ESP += CUInt(imm16)
        End If
    End Sub

    Private Sub ExecuteLIA()
        Dim imm32 As UShort = Mem.ReadDWord(EIP)
        EIP += 4
        IRA = CUInt(imm32)
    End Sub

    Private Sub ExecuteIRET()
        Dim flags As UInteger
        'carica iep dallo stack
        EIP = Mem.ReadDWord(ESP)
        ESP += 4
        'carica il registro flag dallo stack
        flags = Mem.ReadDWord(ESP)
        ESP += 4
        SetEFLAGS(flags)
        'azzera if e interruzione in modo da poter gestire una nuova interruzione
        InterruptFlag = False
        InterruzioneCorrente = False
    End Sub

    Private Sub ExecuteCALL(opcode As Byte)
        Dim size As Integer = If(prefixes.OperandSizeOverride, 16, 32)

        Select Case opcode
            Case &HE8 ' CALL rel16/32 (near, relative)
                Dim displacement As Integer
                If size = 16 Then
                    displacement = CShort(Mem.ReadWord(EIP))
                    EIP += 2
                Else
                    displacement = CInt(Mem.ReadDWord(EIP))
                    EIP += 4
                End If

                ' Push EIP
                If size = 16 Then
                    ESP -= 2
                    Mem.WriteWord(ESP, CUShort(EIP And &HFFFF))
                Else
                    ESP -= 4
                    Mem.WriteDWord(ESP, EIP)
                End If

                ' Calcola nuovo EIP
                EIP += CUInt(displacement)

            Case &H9A ' CALL ptr16:16/32 (far, absolute)
                Dim segment As UShort
                Dim offset As UInteger

                If size = 16 Then
                    offset = Mem.ReadWord(EIP)
                    EIP += 2
                Else
                    offset = Mem.ReadDWord(EIP)
                    EIP += 4
                End If
                segment = Mem.ReadWord(EIP)
                EIP += 2


                ' Push EIP
                If size = 16 Then
                    ESP -= 2
                    Mem.WriteWord(ESP, CUShort(EIP And &HFFFF))
                Else
                    ESP -= 4
                    Mem.WriteDWord(ESP, EIP)
                End If

                ' Imposta nuovo CS:EIP
                EIP = offset

            Case &HFF ' CALL r/m16/32 (near, absolute indirect)
                Dim modRM As ModRMByte = DecodeModRM()

                Dim target As UInteger
                If modRM._Mod = 3 Then
                    ' Registro
                    target = If(size = 16, ReadRegister16(modRM.RM), ReadRegister32(modRM.RM))
                Else
                    ' Memoria
                    Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                    target = If(size = 16, Mem.ReadWord(effectiveAddress),
                                       Mem.ReadDWord(effectiveAddress))
                End If

                ' Push EIP
                If size = 16 Then
                    ESP -= 2
                    Mem.WriteWord(ESP, CUShort(EIP And &HFFFF))
                Else
                    ESP -= 4
                    Mem.WriteDWord(ESP, EIP)
                End If

                ' Set nuovo EIP
                EIP = target

            Case Else
                LogError("Opcode CALL non valido: " & opcode.ToString("X2"))
        End Select
    End Sub

    Private Sub ExecuteJMP(opcode As Byte)
        Dim size As Integer = If(prefixes.OperandSizeOverride, 16, 32)

        Select Case opcode
            Case &HEB ' JMP rel8 (short jump)
                Dim displacement As SByte = CSByte(Mem.ReadByte(EIP))
                EIP += 1
                EIP += CUInt(displacement)

            Case &HE9 ' JMP rel16/32 (near relative jump)
                Dim displacement As Integer
                If size = 16 Then
                    displacement = CShort(Mem.ReadWord(EIP))
                    EIP += 2
                Else
                    displacement = CInt(Mem.ReadDWord(EIP))
                    EIP += 4
                End If
                EIP += CUInt(displacement)

            Case &HEA ' JMP ptr16:16/32 (far jump)
                Dim segment As UShort
                Dim offset As UInteger

                If size = 16 Then
                    offset = Mem.ReadWord(EIP)
                    EIP += 2
                Else
                    offset = Mem.ReadDWord(EIP)
                    EIP += 4
                End If
                segment = Mem.ReadWord(EIP)
                EIP += 2

                ' Set nuovo EIP
                EIP = offset

            Case &HFF ' JMP r/m16/32 (near absolute indirect jump)
                Dim modRM As ModRMByte = DecodeModRM()

                If modRM.Reg = 4 Then ' Near indirect jump
                    Dim target As UInteger
                    If modRM._Mod = 3 Then
                        ' Registro
                        target = If(size = 16, ReadRegister16(modRM.RM), ReadRegister32(modRM.RM))
                    Else
                        ' Memoria
                        Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                        target = If(size = 16, Mem.ReadWord(effectiveAddress),
                                           Mem.ReadDWord(effectiveAddress))
                    End If
                    EIP = target

                ElseIf modRM.Reg = 5 Then ' Far indirect jump
                    If modRM._Mod = 3 Then
                        LogError("Invalid addressing mode for far indirect JMP")
                    Else
                        Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                        Dim offset As UInteger
                        Dim segment As UShort

                        If size = 16 Then
                            offset = Mem.ReadWord(effectiveAddress)
                            segment = Mem.ReadWord(effectiveAddress + 2)
                        Else
                            offset = Mem.ReadDWord(effectiveAddress)
                            segment = Mem.ReadWord(effectiveAddress + 4)
                        End If

                        ' Set nuovo EIP
                        EIP = offset
                    End If
                Else
                    LogError("Invalid ModR/M.Reg for JMP instruction")
                End If

            Case Else
                LogError("Opcode JMP non valido: " & opcode.ToString("X2"))
        End Select
    End Sub

    Private Sub ExecuteLOOP(opcode As Byte)
        Dim displacement As SByte = CSByte(Mem.ReadByte(EIP))
        EIP += 1
        ECX -= 1

        Select Case opcode
            Case &HE2  ' LOOP
                If ECX <> 0 Then
                    EIP += CUInt(displacement)
                End If
            Case &HE1  ' LOOPE/LOOPZ
                If ECX <> 0 AndAlso ZeroFlag Then
                    EIP += CUInt(displacement)
                End If
            Case &HE0  ' LOOPNE/LOOPNZ
                If ECX <> 0 AndAlso Not ZeroFlag Then
                    EIP += CUInt(displacement)
                End If
            Case Else
                LogError("Opcode LOOP non valido: " & opcode.ToString("X2"))
        End Select
    End Sub

    Private Sub ExecuteBT(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()
        Dim size As Integer = If(prefixes.OperandSizeOverride, 16, 32)
        Dim bitIndex As UInteger

        Select Case opcode
            Case &HA3  ' BT r/m16/32, r16/32
                If modRM._Mod = 3 Then
                    Dim value As UInteger = If(size = 16, ReadRegister16(modRM.RM), ReadRegister32(modRM.RM))
                    bitIndex = If(size = 16, ReadRegister16(modRM.Reg), ReadRegister32(modRM.Reg))
                    bitIndex = bitIndex And (size - 1)  ' Maschera per 16 o 32 bit
                    CarryFlag = CBool((value >> CInt(bitIndex)) And 1)
                Else
                    Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                    bitIndex = If(size = 16, ReadRegister16(modRM.Reg), ReadRegister32(modRM.Reg))
                    Dim byteOffset As UInteger = bitIndex \ 8
                    Dim bitOffset As Byte = CByte(bitIndex And 7)
                    Dim value As Byte = Mem.ReadByte(effectiveAddress + byteOffset)
                    CarryFlag = CBool((value >> bitOffset) And 1)
                End If

            Case &HBA  ' BT r/m16/32, imm8
                Dim imm8 As Byte = Mem.ReadByte(EIP)
                EIP += 1
                If modRM._Mod = 3 Then
                    Dim value As UInteger = If(size = 16, ReadRegister16(modRM.RM), ReadRegister32(modRM.RM))
                    bitIndex = CUInt(imm8 And (size - 1))
                    CarryFlag = CBool((value >> CInt(bitIndex)) And 1)
                Else
                    Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                    Dim byteOffset As UInteger = CUInt(imm8) \ 8
                    Dim bitOffset As Byte = CByte(imm8 And 7)
                    Dim value As Byte = Mem.ReadByte(effectiveAddress + byteOffset)
                    CarryFlag = CBool((value >> bitOffset) And 1)
                End If

            Case Else
                LogError("Opcode BT non valido: " & opcode.ToString("X2"))
        End Select
    End Sub

    Private Sub ExecuteBTC(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()
        Dim size As Integer = If(prefixes.OperandSizeOverride, 16, 32)
        Dim bitIndex As UInteger
        Select Case opcode
            Case &HBB  ' BTC r/m16/32, r16/32
                If modRM._Mod = 3 Then
                    Dim value As UInteger = If(size = 16, ReadRegister16(modRM.RM), ReadRegister32(modRM.RM))
                    bitIndex = If(size = 16, ReadRegister16(modRM.Reg), ReadRegister32(modRM.Reg))
                    bitIndex = bitIndex And (size - 1)  ' Maschera per 16 o 32 bit
                    CarryFlag = CBool((value >> CInt(bitIndex)) And 1)
                    value = value Xor (1UI << CInt(bitIndex))
                    If size = 16 Then
                        WriteRegister16(modRM.RM, CUShort(value))
                    Else
                        WriteRegister32(modRM.RM, value)
                    End If
                Else
                    Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                    bitIndex = If(size = 16, ReadRegister16(modRM.Reg), ReadRegister32(modRM.Reg))
                    Dim byteOffset As UInteger = bitIndex \ 8
                    Dim bitOffset As Byte = CByte(bitIndex And 7)
                    Dim value As Byte = Mem.ReadByte(effectiveAddress + byteOffset)
                    CarryFlag = CBool((value >> bitOffset) And 1)
                    value = value Xor CByte(1 << bitOffset)
                    Mem.WriteByte(effectiveAddress + byteOffset, value)
                End If
            Case &HBA  ' BTC r/m16/32, imm8
                Dim imm8 As Byte = Mem.ReadByte(EIP)
                EIP += 1
                If modRM._Mod = 3 Then
                    Dim value As UInteger = If(size = 16, ReadRegister16(modRM.RM), ReadRegister32(modRM.RM))
                    bitIndex = CUInt(imm8 And (size - 1))
                    CarryFlag = CBool((value >> CInt(bitIndex)) And 1)
                    value = value Xor (1UI << CInt(bitIndex))
                    If size = 16 Then
                        WriteRegister16(modRM.RM, CUShort(value))
                    Else
                        WriteRegister32(modRM.RM, value)
                    End If
                Else
                    Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                    Dim byteOffset As UInteger = CUInt(imm8) \ 8
                    Dim bitOffset As Byte = CByte(imm8 And 7)
                    Dim value As Byte = Mem.ReadByte(effectiveAddress + byteOffset)
                    CarryFlag = CBool((value >> bitOffset) And 1)
                    value = value Xor CByte(1 << bitOffset)
                    Mem.WriteByte(effectiveAddress + byteOffset, value)
                End If
            Case Else
                LogError("Opcode BTC non valido: " & opcode.ToString("X2"))
        End Select
    End Sub

    Private Sub ExecuteBTR(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()
        Dim size As Integer = If(prefixes.OperandSizeOverride, 16, 32)
        Dim bitIndex As UInteger
        Select Case opcode
            Case &HB3  ' BTR r/m16/32, r16/32
                If modRM._Mod = 3 Then
                    Dim value As UInteger = If(size = 16, ReadRegister16(modRM.RM), ReadRegister32(modRM.RM))
                    bitIndex = If(size = 16, ReadRegister16(modRM.Reg), ReadRegister32(modRM.Reg))
                    bitIndex = bitIndex And (size - 1)  ' Maschera per 16 o 32 bit
                    CarryFlag = CBool((value >> CInt(bitIndex)) And 1)
                    value = value And (Not (1UI << CInt(bitIndex)))
                    If size = 16 Then
                        WriteRegister16(modRM.RM, CUShort(value))
                    Else
                        WriteRegister32(modRM.RM, value)
                    End If
                Else
                    Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                    bitIndex = If(size = 16, ReadRegister16(modRM.Reg), ReadRegister32(modRM.Reg))
                    Dim byteOffset As UInteger = bitIndex \ 8
                    Dim bitOffset As Byte = CByte(bitIndex And 7)
                    Dim value As Byte = Mem.ReadByte(effectiveAddress + byteOffset)
                    CarryFlag = CBool((value >> bitOffset) And 1)
                    value = value And CByte(Not (1 << bitOffset))
                    Mem.WriteByte(effectiveAddress + byteOffset, value)
                End If
            Case &HBA  ' BTR r/m16/32, imm8
                Dim imm8 As Byte = Mem.ReadByte(EIP)
                EIP += 1
                If modRM._Mod = 3 Then
                    Dim value As UInteger = If(size = 16, ReadRegister16(modRM.RM), ReadRegister32(modRM.RM))
                    bitIndex = CUInt(imm8 And (size - 1))
                    CarryFlag = CBool((value >> CInt(bitIndex)) And 1)
                    value = value And (Not (1UI << CInt(bitIndex)))
                    If size = 16 Then
                        WriteRegister16(modRM.RM, CUShort(value))
                    Else
                        WriteRegister32(modRM.RM, value)
                    End If
                Else
                    Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                    Dim byteOffset As UInteger = CUInt(imm8) \ 8
                    Dim bitOffset As Byte = CByte(imm8 And 7)
                    Dim value As Byte = Mem.ReadByte(effectiveAddress + byteOffset)
                    CarryFlag = CBool((value >> bitOffset) And 1)
                    value = value And CByte(Not (1 << bitOffset))
                    Mem.WriteByte(effectiveAddress + byteOffset, value)
                End If
            Case Else
                LogError("Opcode BTR non valido: " & opcode.ToString("X2"))
        End Select
    End Sub

    Private Sub ExecuteBTS(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()
        Dim size As Integer = If(prefixes.OperandSizeOverride, 16, 32)
        Dim bitIndex As UInteger
        Select Case opcode
            Case &HAB  ' BTS r/m16/32, r16/32
                If modRM._Mod = 3 Then
                    Dim value As UInteger = If(size = 16, ReadRegister16(modRM.RM), ReadRegister32(modRM.RM))
                    bitIndex = If(size = 16, ReadRegister16(modRM.Reg), ReadRegister32(modRM.Reg))
                    bitIndex = bitIndex And (size - 1)  ' Maschera per 16 o 32 bit
                    CarryFlag = CBool((value >> CInt(bitIndex)) And 1)
                    value = value Or (1UI << CInt(bitIndex))
                    If size = 16 Then
                        WriteRegister16(modRM.RM, CUShort(value))
                    Else
                        WriteRegister32(modRM.RM, value)
                    End If
                Else
                    Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                    bitIndex = If(size = 16, ReadRegister16(modRM.Reg), ReadRegister32(modRM.Reg))
                    Dim byteOffset As UInteger = bitIndex \ 8
                    Dim bitOffset As Byte = CByte(bitIndex And 7)
                    Dim value As Byte = Mem.ReadByte(effectiveAddress + byteOffset)
                    CarryFlag = CBool((value >> bitOffset) And 1)
                    value = value Or CByte(1 << bitOffset)
                    Mem.WriteByte(effectiveAddress + byteOffset, value)
                End If
            Case &HBA  ' BTS r/m16/32, imm8
                Dim imm8 As Byte = Mem.ReadByte(EIP)
                EIP += 1
                If modRM._Mod = 3 Then
                    Dim value As UInteger = If(size = 16, ReadRegister16(modRM.RM), ReadRegister32(modRM.RM))
                    bitIndex = CUInt(imm8 And (size - 1))
                    CarryFlag = CBool((value >> CInt(bitIndex)) And 1)
                    value = value Or (1UI << CInt(bitIndex))
                    If size = 16 Then
                        WriteRegister16(modRM.RM, CUShort(value))
                    Else
                        WriteRegister32(modRM.RM, value)
                    End If
                Else
                    Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                    Dim byteOffset As UInteger = CUInt(imm8) \ 8
                    Dim bitOffset As Byte = CByte(imm8 And 7)
                    Dim value As Byte = Mem.ReadByte(effectiveAddress + byteOffset)
                    CarryFlag = CBool((value >> bitOffset) And 1)
                    value = value Or CByte(1 << bitOffset)
                    Mem.WriteByte(effectiveAddress + byteOffset, value)
                End If
            Case Else
                LogError("Opcode BTS non valido: " & opcode.ToString("X2"))
        End Select
    End Sub

    Private Sub ExecuteNEG(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()
        Dim size As Integer = If(opcode = &HF6, 8, If(prefixes.OperandSizeOverride, 16, 32))
        Dim value As UInteger

        If modRM._Mod = 3 Then
            Select Case size
                Case 8
                    value = ReadRegister8(modRM.RM)
                Case 16
                    value = ReadRegister16(modRM.RM)
                Case 32
                    value = ReadRegister32(modRM.RM)
            End Select

            CarryFlag = (value <> 0)
            value = CUInt(0 - value)

            Select Case size
                Case 8
                    WriteRegister8(modRM.RM, CByte(value))
                Case 16
                    WriteRegister16(modRM.RM, CUShort(value))
                Case 32
                    WriteRegister32(modRM.RM, value)
            End Select
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)

            Select Case size
                Case 8
                    value = Mem.ReadByte(effectiveAddress)
                Case 16
                    value = Mem.ReadWord(effectiveAddress)
                Case 32
                    value = Mem.ReadDWord(effectiveAddress)
            End Select

            CarryFlag = (value <> 0)
            value = CUInt(0 - value)

            Select Case size
                Case 8
                    Mem.WriteByte(effectiveAddress, CByte(value))
                Case 16
                    Mem.WriteWord(effectiveAddress, CUShort(value))
                Case 32
                    Mem.WriteDWord(effectiveAddress, value)
            End Select
        End If

        SetFlagsAfterOperation(value, size)
    End Sub

    Private Sub ExecuteNOT(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()
        Dim size As Integer = If(opcode = &HF6, 8, If(prefixes.OperandSizeOverride, 16, 32))

        If modRM._Mod = 3 Then
            Dim value As UInteger
            Select Case size
                Case 8
                    value = ReadRegister8(modRM.RM)
                Case 16
                    value = ReadRegister16(modRM.RM)
                Case 32
                    value = ReadRegister32(modRM.RM)
            End Select

            value = Not value

            Select Case size
                Case 8
                    WriteRegister8(modRM.RM, CByte(value))
                Case 16
                    WriteRegister16(modRM.RM, CUShort(value))
                Case 32
                    WriteRegister32(modRM.RM, value)
            End Select
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            Dim value As UInteger

            Select Case size
                Case 8
                    value = Mem.ReadByte(effectiveAddress)
                Case 16
                    value = Mem.ReadWord(effectiveAddress)
                Case 32
                    value = Mem.ReadDWord(effectiveAddress)
            End Select

            value = Not value

            Select Case size
                Case 8
                    Mem.WriteByte(effectiveAddress, CByte(value))
                Case 16
                    Mem.WriteWord(effectiveAddress, CUShort(value))
                Case 32
                    Mem.WriteDWord(effectiveAddress, value)
            End Select
        End If

        ' NOT non influisce sui flag
    End Sub

    Private Sub ExecuteSHLD(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()
        Dim size As Integer = If(prefixes.OperandSizeOverride, 16, 32)
        Dim count As Byte

        If opcode = &HA4 Then
            count = Mem.ReadByte(EIP)
            EIP += 1
        Else
            count = CL
        End If
        count = CByte(count And If(size = 16, &HF, &H1F))

        Dim destination, source As UInteger
        If modRM._Mod = 3 Then
            destination = If(size = 16, ReadRegister16(modRM.RM), ReadRegister32(modRM.RM))
            source = If(size = 16, ReadRegister16(modRM.Reg), ReadRegister32(modRM.Reg))
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            destination = If(size = 16, Mem.ReadWord(effectiveAddress), Mem.ReadDWord(effectiveAddress))
            source = If(size = 16, ReadRegister16(modRM.Reg), ReadRegister32(modRM.Reg))
        End If

        If count > 0 Then
            Dim mask As UInteger = If(size = 16, &HFFFF, &HFFFFFFFFUI)
            Dim result As UInteger = ((destination << count) Or (source >> (size - count))) And mask
            CarryFlag = CBool((destination >> (size - count)) And 1)

            If modRM._Mod = 3 Then
                If size = 16 Then
                    WriteRegister16(modRM.RM, CUShort(result))
                Else
                    WriteRegister32(modRM.RM, result)
                End If
            Else
                If size = 16 Then
                    Mem.WriteWord(DecodeEffectiveAddress(modRM), CUShort(result))
                Else
                    Mem.WriteDWord(DecodeEffectiveAddress(modRM), result)
                End If
            End If

            SetFlagsAfterOperation(result, size)
        End If
    End Sub

    Private Sub ExecuteSHRD(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()
        Dim size As Integer = If(prefixes.OperandSizeOverride, 16, 32)
        Dim count As Byte

        If opcode = &HAC Then
            count = Mem.ReadByte(EIP)
            EIP += 1
        Else
            count = CL
        End If
        count = CByte(count And If(size = 16, &HF, &H1F))

        Dim destination, source As UInteger
        If modRM._Mod = 3 Then
            destination = If(size = 16, ReadRegister16(modRM.RM), ReadRegister32(modRM.RM))
            source = If(size = 16, ReadRegister16(modRM.Reg), ReadRegister32(modRM.Reg))
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            destination = If(size = 16, Mem.ReadWord(effectiveAddress), Mem.ReadDWord(effectiveAddress))
            source = If(size = 16, ReadRegister16(modRM.Reg), ReadRegister32(modRM.Reg))
        End If

        If count > 0 Then
            Dim mask As UInteger = If(size = 16, &HFFFF, &HFFFFFFFFUI)
            Dim result As UInteger = ((destination >> count) Or (source << (size - count))) And mask
            CarryFlag = CBool((destination >> (count - 1)) And 1)

            If modRM._Mod = 3 Then
                If size = 16 Then
                    WriteRegister16(modRM.RM, CUShort(result))
                Else
                    WriteRegister32(modRM.RM, result)
                End If
            Else
                If size = 16 Then
                    Mem.WriteWord(DecodeEffectiveAddress(modRM), CUShort(result))
                Else
                    Mem.WriteDWord(DecodeEffectiveAddress(modRM), result)
                End If
            End If

            SetFlagsAfterOperation(result, size)
        End If
    End Sub

    Private Sub ExecuteSHL(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()
        Dim size As Integer = If(opcode And 1, If(prefixes.OperandSizeOverride, 16, 32), 8)
        Dim count As Byte
        If (opcode And &HFE) = &HC0 Then
            count = 1
        ElseIf (opcode And &HFE) = &HD0 Then
            count = CL
        Else
            count = Mem.ReadByte(EIP)
            EIP += 1
        End If
        If size = 16 Then
            count = CByte(count And &HF)
        ElseIf size = 32 Then
            count = CByte(count And &H1F)
        End If
        Dim value As UInteger
        If modRM._Mod = 3 Then
            value = If(size = 8, ReadRegister8(modRM.RM), If(size = 16, ReadRegister16(modRM.RM), ReadRegister32(modRM.RM)))
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            value = If(size = 8, Mem.ReadByte(effectiveAddress),
                   If(size = 16, Mem.ReadWord(effectiveAddress),
                      Mem.ReadDWord(effectiveAddress)))
        End If
        If count > 0 Then
            CarryFlag = CBool((value >> (size - count)) And 1)
            value <<= count
            WriteShiftResult(modRM, size, value)
            SetFlagsAfterOperation(value, size)
        End If
    End Sub

    Private Sub ExecuteSHR(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()
        Dim size As Integer = If(opcode And 1, If(prefixes.OperandSizeOverride, 16, 32), 8)
        Dim count As Byte
        If (opcode And &HFE) = &HC0 Then
            count = 1
        ElseIf (opcode And &HFE) = &HD0 Then
            count = CL
        Else
            count = Mem.ReadByte(EIP)
            EIP += 1
        End If
        If size = 16 Then
            count = CByte(count And &HF)
        ElseIf size = 32 Then
            count = CByte(count And &H1F)
        End If
        Dim value As UInteger
        If modRM._Mod = 3 Then
            value = If(size = 8, ReadRegister8(modRM.RM), If(size = 16, ReadRegister16(modRM.RM), ReadRegister32(modRM.RM)))
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            value = If(size = 8, Mem.ReadByte(effectiveAddress),
                   If(size = 16, Mem.ReadWord(effectiveAddress),
                      Mem.ReadDWord(effectiveAddress)))
        End If
        If count > 0 Then
            CarryFlag = CBool((value >> (count - 1)) And 1)
            value >>= count
            WriteShiftResult(modRM, size, value)
            SetFlagsAfterOperation(value, size)
        End If
    End Sub

    Private Sub ExecuteSAR(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()
        Dim size As Integer = If(opcode And 1, If(prefixes.OperandSizeOverride, 16, 32), 8)
        Dim count As Byte
        If (opcode And &HFE) = &HC0 Then
            count = 1
        ElseIf (opcode And &HFE) = &HD0 Then
            count = CL
        Else
            count = Mem.ReadByte(EIP)
            EIP += 1
        End If
        If size = 16 Then
            count = CByte(count And &HF)
        ElseIf size = 32 Then
            count = CByte(count And &H1F)
        End If
        Dim value As UInteger
        If modRM._Mod = 3 Then
            value = If(size = 8, ReadRegister8(modRM.RM), If(size = 16, ReadRegister16(modRM.RM), ReadRegister32(modRM.RM)))
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            value = If(size = 8, Mem.ReadByte(effectiveAddress),
                   If(size = 16, Mem.ReadWord(effectiveAddress),
                      Mem.ReadDWord(effectiveAddress)))
        End If
        If count > 0 Then
            CarryFlag = CBool((value >> (count - 1)) And 1)
            Dim signBit As UInteger = value And (1UI << (size - 1))
            value = (value >> count) Or (If(signBit <> 0, &HFFFFFFFFUI << (size - count), 0))
            WriteShiftResult(modRM, size, value)
            SetFlagsAfterOperation(value, size)
        End If
    End Sub

    Private Sub WriteShiftResult(modRM As ModRMByte, size As Integer, value As UInteger)
        If modRM._Mod = 3 Then
            Select Case size
                Case 8
                    WriteRegister8(modRM.RM, CByte(value))
                Case 16
                    WriteRegister16(modRM.RM, CUShort(value))
                Case 32
                    WriteRegister32(modRM.RM, value)
            End Select
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            Select Case size
                Case 8
                    Mem.WriteByte(effectiveAddress, CByte(value))
                Case 16
                    Mem.WriteWord(effectiveAddress, CUShort(value))
                Case 32
                    Mem.WriteDWord(effectiveAddress, value)
            End Select
        End If
    End Sub

    Private Sub ExecuteRCL(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()
        Dim size As Integer = If(opcode And 1, If(prefixes.OperandSizeOverride, 16, 32), 8)
        Dim count As Byte

        If (opcode And &HFE) = &HC0 Then
            count = 1
        ElseIf (opcode And &HFE) = &HD0 Then
            count = CL
        Else
            count = Mem.ReadByte(EIP)
            EIP += 1
        End If

        count = CByte(count Mod (size + 1))

        Dim value As UInteger
        If modRM._Mod = 3 Then
            value = If(size = 8, ReadRegister8(modRM.RM), If(size = 16, ReadRegister16(modRM.RM), ReadRegister32(modRM.RM)))
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            value = If(size = 8, Mem.ReadByte(effectiveAddress),
                       If(size = 16, Mem.ReadWord(effectiveAddress),
                          Mem.ReadDWord(effectiveAddress)))
        End If

        If count > 0 Then
            Dim mask As UInteger = If(size = 8, &HFF, If(size = 16, &HFFFF, &HFFFFFFFFUI))
            Dim carryMask As UInteger = 1UI << (size - 1)
            Dim result As UInteger = value
            Dim oldCarry As Boolean = CarryFlag

            For i As Integer = 1 To count
                Dim newCarry As Boolean = CBool(result And carryMask)
                result = ((result << 1) Or (If(oldCarry, 1, 0))) And mask
                oldCarry = newCarry
            Next

            CarryFlag = oldCarry

            If modRM._Mod = 3 Then
                Select Case size
                    Case 8
                        WriteRegister8(modRM.RM, CByte(result))
                    Case 16
                        WriteRegister16(modRM.RM, CUShort(result))
                    Case 32
                        WriteRegister32(modRM.RM, result)
                End Select
            Else
                Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                Select Case size
                    Case 8
                        Mem.WriteByte(effectiveAddress, CByte(result))
                    Case 16
                        Mem.WriteWord(effectiveAddress, CUShort(result))
                    Case 32
                        Mem.WriteDWord(effectiveAddress, result)
                End Select
            End If

            ' RCL influisce solo su CF e OF
            OverflowFlag = CBool((result Xor (result << 1)) And carryMask)
        End If
    End Sub

    Private Sub ExecuteRCR(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()
        Dim size As Integer = If(opcode And 1, If(prefixes.OperandSizeOverride, 16, 32), 8)
        Dim count As Byte

        If (opcode And &HFE) = &HC0 Then
            count = 1
        ElseIf (opcode And &HFE) = &HD0 Then
            count = CL
        Else
            count = Mem.ReadByte(EIP)
            EIP += 1
        End If

        count = CByte(count Mod (size + 1))

        Dim value As UInteger
        If modRM._Mod = 3 Then
            value = If(size = 8, ReadRegister8(modRM.RM), If(size = 16, ReadRegister16(modRM.RM), ReadRegister32(modRM.RM)))
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            value = If(size = 8, Mem.ReadByte(effectiveAddress),
                       If(size = 16, Mem.ReadWord(effectiveAddress),
                          Mem.ReadDWord(effectiveAddress)))
        End If

        If count > 0 Then
            Dim mask As UInteger = If(size = 8, &HFF, If(size = 16, &HFFFF, &HFFFFFFFFUI))
            Dim result As UInteger = value
            Dim oldCarry As Boolean = CarryFlag

            For i As Integer = 1 To count
                Dim newCarry As Boolean = CBool(result And 1)
                result = ((result >> 1) Or (If(oldCarry, 1UI << (size - 1), 0))) And mask
                oldCarry = newCarry
            Next

            CarryFlag = oldCarry

            If modRM._Mod = 3 Then
                Select Case size
                    Case 8
                        WriteRegister8(modRM.RM, CByte(result))
                    Case 16
                        WriteRegister16(modRM.RM, CUShort(result))
                    Case 32
                        WriteRegister32(modRM.RM, result)
                End Select
            Else
                Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                Select Case size
                    Case 8
                        Mem.WriteByte(effectiveAddress, CByte(result))
                    Case 16
                        Mem.WriteWord(effectiveAddress, CUShort(result))
                    Case 32
                        Mem.WriteDWord(effectiveAddress, result)
                End Select
            End If

            ' RCR influisce solo su CF e OF
            OverflowFlag = CBool((result Xor (result >> 1)) And (1UI << (size - 1)))
        End If
    End Sub

    Private Sub ExecuteROL(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()
        Dim size As Integer = If(opcode And 1, If(prefixes.OperandSizeOverride, 16, 32), 8)
        Dim count As Byte
        If (opcode And &HFE) = &HC0 Then
            count = 1
        ElseIf (opcode And &HFE) = &HD0 Then
            count = CL
        Else
            count = Mem.ReadByte(EIP)
            EIP += 1
        End If
        count = CByte(count Mod size)
        Dim value As UInteger
        If modRM._Mod = 3 Then
            value = If(size = 8, ReadRegister8(modRM.RM), If(size = 16, ReadRegister16(modRM.RM), ReadRegister32(modRM.RM)))
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            value = If(size = 8, Mem.ReadByte(effectiveAddress),
                   If(size = 16, Mem.ReadWord(effectiveAddress),
                      Mem.ReadDWord(effectiveAddress)))
        End If
        If count > 0 Then
            Dim mask As UInteger = If(size = 8, &HFF, If(size = 16, &HFFFF, &HFFFFFFFFUI))
            Dim result As UInteger = ((value << count) Or (value >> (size - count))) And mask
            CarryFlag = CBool(result And 1)
            WriteRotateResult(modRM, size, result)
            OverflowFlag = CBool((result Xor (result << 1)) And (1UI << (size - 1)))
        End If
    End Sub

    Private Sub ExecuteROR(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()
        Dim size As Integer = If(opcode And 1, If(prefixes.OperandSizeOverride, 16, 32), 8)
        Dim count As Byte
        If (opcode And &HFE) = &HC0 Then
            count = 1
        ElseIf (opcode And &HFE) = &HD0 Then
            count = CL
        Else
            count = Mem.ReadByte(EIP)
            EIP += 1
        End If
        count = CByte(count Mod size)
        Dim value As UInteger
        If modRM._Mod = 3 Then
            value = If(size = 8, ReadRegister8(modRM.RM), If(size = 16, ReadRegister16(modRM.RM), ReadRegister32(modRM.RM)))
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            value = If(size = 8, Mem.ReadByte(effectiveAddress),
                   If(size = 16, Mem.ReadWord(effectiveAddress),
                      Mem.ReadDWord(effectiveAddress)))
        End If
        If count > 0 Then
            Dim mask As UInteger = If(size = 8, &HFF, If(size = 16, &HFFFF, &HFFFFFFFFUI))
            Dim result As UInteger = ((value >> count) Or (value << (size - count))) And mask
            CarryFlag = CBool(result And (1UI << (size - 1)))
            WriteRotateResult(modRM, size, result)
            OverflowFlag = CBool((result Xor (result >> 1)) And (1UI << (size - 1)))
        End If
    End Sub

    Private Sub WriteRotateResult(modRM As ModRMByte, size As Integer, result As UInteger)
        If modRM._Mod = 3 Then
            Select Case size
                Case 8
                    WriteRegister8(modRM.RM, CByte(result))
                Case 16
                    WriteRegister16(modRM.RM, CUShort(result))
                Case 32
                    WriteRegister32(modRM.RM, result)
            End Select
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            Select Case size
                Case 8
                    Mem.WriteByte(effectiveAddress, CByte(result))
                Case 16
                    Mem.WriteWord(effectiveAddress, CUShort(result))
                Case 32
                    Mem.WriteDWord(effectiveAddress, result)
            End Select
        End If
    End Sub

    Private Sub ExecuteTEST(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()
        Dim size As Integer = If(opcode And 1, If(prefixes.OperandSizeOverride, 16, 32), 8)

        Dim value1, value2 As UInteger

        If opcode = &H84 Or opcode = &H85 Then ' TEST r/m8, r8 or TEST r/m16/32, r16/32
            If modRM._Mod = 3 Then
                value1 = If(size = 8, ReadRegister8(modRM.RM), If(size = 16, ReadRegister16(modRM.RM), ReadRegister32(modRM.RM)))
            Else
                Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                value1 = If(size = 8, Mem.ReadByte(effectiveAddress),
                           If(size = 16, Mem.ReadWord(effectiveAddress),
                              Mem.ReadDWord(effectiveAddress)))
            End If
            value2 = If(size = 8, ReadRegister8(modRM.Reg), If(size = 16, ReadRegister16(modRM.Reg), ReadRegister32(modRM.Reg)))
        Else ' TEST AL/AX/EAX, imm8/16/32 or TEST r/m8, imm8 or TEST r/m16/32, imm16/32
            If opcode = &HA8 Or opcode = &HA9 Then
                value1 = If(size = 8, AL, If(size = 16, AX, EAX))
            Else
                If modRM._Mod = 3 Then
                    value1 = If(size = 8, ReadRegister8(modRM.RM), If(size = 16, ReadRegister16(modRM.RM), ReadRegister32(modRM.RM)))
                Else
                    Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                    value1 = If(size = 8, Mem.ReadByte(effectiveAddress),
                               If(size = 16, Mem.ReadWord(effectiveAddress),
                                  Mem.ReadDWord(effectiveAddress)))
                End If
            End If
            value2 = If(size = 8, Mem.ReadByte(EIP), If(size = 16, Mem.ReadWord(EIP), Mem.ReadDWord(EIP)))
            EIP += CUInt(size \ 8)
        End If

        Dim result As UInteger = value1 And value2
        SetFlagsAfterLogicalOperation(result, size)
    End Sub

    Private Sub ExecuteOR(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()
        Dim size As Integer = If(opcode And 1, If(prefixes.OperandSizeOverride, 16, 32), 8)

        Dim value1, value2, result As UInteger

        Select Case opcode And &HFE
            Case &H0, &H8 ' OR r/m8, r8 or OR r/m16/32, r16/32
                If modRM._Mod = 3 Then
                    value1 = If(size = 8, ReadRegister8(modRM.RM), If(size = 16, ReadRegister16(modRM.RM), ReadRegister32(modRM.RM)))
                Else
                    Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                    value1 = If(size = 8, Mem.ReadByte(effectiveAddress),
                           If(size = 16, Mem.ReadWord(effectiveAddress),
                              Mem.ReadDWord(effectiveAddress)))
                End If
                value2 = If(size = 8, ReadRegister8(modRM.Reg), If(size = 16, ReadRegister16(modRM.Reg), ReadRegister32(modRM.Reg)))

                result = value1 Or value2

                If modRM._Mod = 3 Then
                    If size = 8 Then
                        WriteRegister8(modRM.RM, CByte(result))
                    ElseIf size = 16 Then
                        WriteRegister16(modRM.RM, CUShort(result))
                    Else
                        WriteRegister32(modRM.RM, result)
                    End If
                Else
                    Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                    If size = 8 Then
                        Mem.WriteByte(effectiveAddress, CByte(result))
                    ElseIf size = 16 Then
                        Mem.WriteWord(effectiveAddress, CUShort(result))
                    Else
                        Mem.WriteDWord(effectiveAddress, result)
                    End If
                End If

            Case &H2, &HA ' OR r8, r/m8 or OR r16/32, r/m16/32
                value1 = If(size = 8, ReadRegister8(modRM.Reg), If(size = 16, ReadRegister16(modRM.Reg), ReadRegister32(modRM.Reg)))
                If modRM._Mod = 3 Then
                    value2 = If(size = 8, ReadRegister8(modRM.RM), If(size = 16, ReadRegister16(modRM.RM), ReadRegister32(modRM.RM)))
                Else
                    Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                    value2 = If(size = 8, Mem.ReadByte(effectiveAddress),
                           If(size = 16, Mem.ReadWord(effectiveAddress),
                              Mem.ReadDWord(effectiveAddress)))
                End If

                result = value1 Or value2

                If size = 8 Then
                    WriteRegister8(modRM.Reg, CByte(result))
                ElseIf size = 16 Then
                    WriteRegister16(modRM.Reg, CUShort(result))
                Else
                    WriteRegister32(modRM.Reg, result)
                End If

            Case Else ' OR AL/AX/EAX, imm8/16/32
                value1 = If(size = 8, AL, If(size = 16, AX, EAX))
                value2 = If(size = 8, Mem.ReadByte(EIP), If(size = 16, Mem.ReadWord(EIP), Mem.ReadDWord(EIP)))
                EIP += CUInt(size \ 8)

                result = value1 Or value2

                If size = 8 Then
                    AL = CByte(result)
                ElseIf size = 16 Then
                    AX = CUShort(result)
                Else
                    EAX = result
                End If
        End Select

        SetFlagsAfterLogicalOperation(result, size)
    End Sub

    Private Sub ExecuteXOR(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()
        Dim size As Integer = If(opcode And 1, If(prefixes.OperandSizeOverride, 16, 32), 8)

        Dim value1, value2, result As UInteger

        Select Case opcode And &HFE
            Case &H0, &H8 ' XOR r/m8, r8 or XOR r/m16/32, r16/32
                If modRM._Mod = 3 Then
                    value1 = If(size = 8, ReadRegister8(modRM.RM), If(size = 16, ReadRegister16(modRM.RM), ReadRegister32(modRM.RM)))
                Else
                    Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                    value1 = If(size = 8, Mem.ReadByte(effectiveAddress),
                           If(size = 16, Mem.ReadWord(effectiveAddress),
                              Mem.ReadDWord(effectiveAddress)))
                End If
                value2 = If(size = 8, ReadRegister8(modRM.Reg), If(size = 16, ReadRegister16(modRM.Reg), ReadRegister32(modRM.Reg)))

                result = value1 Xor value2

                If modRM._Mod = 3 Then
                    If size = 8 Then
                        WriteRegister8(modRM.RM, CByte(result))
                    ElseIf size = 16 Then
                        WriteRegister16(modRM.RM, CUShort(result))
                    Else
                        WriteRegister32(modRM.RM, result)
                    End If
                Else
                    Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                    If size = 8 Then
                        Mem.WriteByte(effectiveAddress, CByte(result))
                    ElseIf size = 16 Then
                        Mem.WriteWord(effectiveAddress, CUShort(result))
                    Else
                        Mem.WriteDWord(effectiveAddress, result)
                    End If
                End If

            Case &H2, &HA ' XOR r8, r/m8 or XOR r16/32, r/m16/32
                value1 = If(size = 8, ReadRegister8(modRM.Reg), If(size = 16, ReadRegister16(modRM.Reg), ReadRegister32(modRM.Reg)))
                If modRM._Mod = 3 Then
                    value2 = If(size = 8, ReadRegister8(modRM.RM), If(size = 16, ReadRegister16(modRM.RM), ReadRegister32(modRM.RM)))
                Else
                    Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                    value2 = If(size = 8, Mem.ReadByte(effectiveAddress),
                           If(size = 16, Mem.ReadWord(effectiveAddress),
                              Mem.ReadDWord(effectiveAddress)))
                End If

                result = value1 Xor value2

                If size = 8 Then
                    WriteRegister8(modRM.Reg, CByte(result))
                ElseIf size = 16 Then
                    WriteRegister16(modRM.Reg, CUShort(result))
                Else
                    WriteRegister32(modRM.Reg, result)
                End If

            Case Else ' XOR AL/AX/EAX, imm8/16/32
                value1 = If(size = 8, AL, If(size = 16, AX, EAX))
                value2 = If(size = 8, Mem.ReadByte(EIP), If(size = 16, Mem.ReadWord(EIP), Mem.ReadDWord(EIP)))
                EIP += CUInt(size \ 8)

                result = value1 Xor value2

                If size = 8 Then
                    AL = CByte(result)
                ElseIf size = 16 Then
                    AX = CUShort(result)
                Else
                    EAX = result
                End If
        End Select

        SetFlagsAfterLogicalOperation(result, size)
    End Sub

    Private Sub ExecuteBSF(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()
        Dim size As Integer = If(prefixes.OperandSizeOverride, 16, 32)
        Dim source As UInteger
        If modRM._Mod = 3 Then
            source = If(size = 16, ReadRegister16(modRM.RM), ReadRegister32(modRM.RM))
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            source = If(size = 16, Mem.ReadWord(effectiveAddress),
                    Mem.ReadDWord(effectiveAddress))
        End If
        If source = 0 Then
            ZeroFlag = True
        Else
            ZeroFlag = False
            Dim bitIndex As Integer = -1
            For i As Integer = 0 To size - 1
                If (source And (1UI << i)) <> 0 Then
                    bitIndex = i
                    Exit For
                End If
            Next
            If bitIndex <> -1 Then
                If size = 16 Then
                    WriteRegister16(modRM.Reg, CUShort(bitIndex))
                Else
                    WriteRegister32(modRM.Reg, CUInt(bitIndex))
                End If
            End If
        End If
        ' BSF influenza solo ZF, gli altri flag sono indefiniti
    End Sub

    Private Sub ExecuteBSR()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim size As Integer = If(prefixes.OperandSizeOverride, 16, 32)
        Dim source As UInteger
        If modRM._Mod = 3 Then
            source = If(size = 16, ReadRegister16(modRM.RM), ReadRegister32(modRM.RM))
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            source = If(size = 16, Mem.ReadWord(effectiveAddress),
                    Mem.ReadDWord(effectiveAddress))
        End If
        If source = 0 Then
            ZeroFlag = True
        Else
            ZeroFlag = False
            Dim bitIndex As Integer = -1
            For i As Integer = size - 1 To 0 Step -1
                If (source And (1UI << i)) <> 0 Then
                    bitIndex = i
                    Exit For
                End If
            Next
            If bitIndex <> -1 Then
                If size = 16 Then
                    WriteRegister16(modRM.Reg, CUShort(bitIndex))
                Else
                    WriteRegister32(modRM.Reg, CUInt(bitIndex))
                End If
            End If
        End If
        ' BSR influenza solo ZF, gli altri flag sono indefiniti
    End Sub

    Private Sub ExecuteINC(opcode As Byte)
        Dim size As Integer = If(opcode >= &H40 AndAlso opcode <= &H47, If(prefixes.OperandSizeOverride, 16, 32), 8)
        Dim modRM As ModRMByte

        If opcode >= &H40 AndAlso opcode <= &H47 Then
            ' INC r16/r32
            Dim regIndex As Byte = CByte(opcode - &H40)
            Dim value As UInteger = If(size = 16, ReadRegister16(regIndex), ReadRegister32(regIndex))
            Dim result As UInteger = value + 1

            If size = 16 Then
                WriteRegister16(regIndex, CUShort(result))
            Else
                WriteRegister32(regIndex, result)
            End If

            SetFlagsAfterIncDec(result, size, True)
        Else
            ' INC r/m8, INC r/m16, INC r/m32
            modRM = DecodeModRM()
            Dim value As UInteger

            If modRM._Mod = 3 Then
                value = If(size = 8, ReadRegister8(modRM.RM), If(size = 16, ReadRegister16(modRM.RM), ReadRegister32(modRM.RM)))
            Else
                Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                value = If(size = 8, Mem.ReadByte(effectiveAddress),
                           If(size = 16, Mem.ReadWord(effectiveAddress),
                              Mem.ReadDWord(effectiveAddress)))
            End If

            Dim result As UInteger = value + 1

            If modRM._Mod = 3 Then
                If size = 8 Then
                    WriteRegister8(modRM.RM, CByte(result))
                ElseIf size = 16 Then
                    WriteRegister16(modRM.RM, CUShort(result))
                Else
                    WriteRegister32(modRM.RM, result)
                End If
            Else
                Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                If size = 8 Then
                    Mem.WriteByte(effectiveAddress, CByte(result))
                ElseIf size = 16 Then
                    Mem.WriteWord(effectiveAddress, CUShort(result))
                Else
                    Mem.WriteDWord(effectiveAddress, result)
                End If
            End If

            SetFlagsAfterIncDec(result, size, True)
        End If
    End Sub

    Private Sub ExecuteDEC(opcode As Byte)
        Dim size As Integer = If(opcode >= &H48 AndAlso opcode <= &H4F, If(prefixes.OperandSizeOverride, 16, 32), 8)
        Dim modRM As ModRMByte

        If opcode >= &H48 AndAlso opcode <= &H4F Then
            ' DEC r16/r32
            Dim regIndex As Byte = CByte(opcode - &H48)
            Dim value As UInteger = If(size = 16, ReadRegister16(regIndex), ReadRegister32(regIndex))
            Dim result As UInteger = value - 1

            If size = 16 Then
                WriteRegister16(regIndex, CUShort(result))
            Else
                WriteRegister32(regIndex, result)
            End If

            SetFlagsAfterIncDec(result, size, False)
        Else
            ' DEC r/m8, DEC r/m16, DEC r/m32
            modRM = DecodeModRM()
            Dim value As UInteger

            If modRM._Mod = 3 Then
                value = If(size = 8, ReadRegister8(modRM.RM), If(size = 16, ReadRegister16(modRM.RM), ReadRegister32(modRM.RM)))
            Else
                Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                value = If(size = 8, Mem.ReadByte(effectiveAddress),
                           If(size = 16, Mem.ReadWord(effectiveAddress),
                              Mem.ReadDWord(effectiveAddress)))
            End If

            Dim result As UInteger = value - 1

            If modRM._Mod = 3 Then
                If size = 8 Then
                    WriteRegister8(modRM.RM, CByte(result))
                ElseIf size = 16 Then
                    WriteRegister16(modRM.RM, CUShort(result))
                Else
                    WriteRegister32(modRM.RM, result)
                End If
            Else
                Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                If size = 8 Then
                    Mem.WriteByte(effectiveAddress, CByte(result))
                ElseIf size = 16 Then
                    Mem.WriteWord(effectiveAddress, CUShort(result))
                Else
                    Mem.WriteDWord(effectiveAddress, result)
                End If
            End If

            SetFlagsAfterIncDec(result, size, False)
        End If
    End Sub

    Private Sub ExecuteSBB(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()

        Select Case opcode
            Case &H18 ' SBB r/m8, r8
                Dim rm8 As Byte = If(modRM._Mod = 3, ReadRegister8(modRM.RM), Mem.ReadByte(DecodeEffectiveAddress(modRM)))
                Dim r8 As Byte = ReadRegister8(modRM.Reg)
                Dim result As UShort = CUShort(rm8) - CUShort(r8) - CUShort(If(CarryFlag, 1, 0))
                CarryFlag = result > &HFF
                If modRM._Mod = 3 Then
                    WriteRegister8(modRM.RM, CByte(result And &HFF))
                Else
                    Mem.WriteByte(DecodeEffectiveAddress(modRM), CByte(result And &HFF))
                End If
                SetFlagsAfterOperation(CByte(result And &HFF), 8)

            Case &H19 ' SBB r/m16, r16 or SBB r/m32, r32
                If prefixes.OperandSizeOverride Then
                    Dim rm16 As UShort = If(modRM._Mod = 3, ReadRegister16(modRM.RM), Mem.ReadWord(DecodeEffectiveAddress(modRM)))
                    Dim r16 As UShort = ReadRegister16(modRM.Reg)
                    Dim result As UInteger = CUInt(rm16) - CUInt(r16) - CUInt(If(CarryFlag, 1, 0))
                    CarryFlag = result > &HFFFF
                    If modRM._Mod = 3 Then
                        WriteRegister16(modRM.RM, CUShort(result And &HFFFF))
                    Else
                        Mem.WriteWord(DecodeEffectiveAddress(modRM), CUShort(result And &HFFFF))
                    End If
                    SetFlagsAfterOperation(CUShort(result And &HFFFF), 16)
                Else
                    Dim rm32 As UInteger = If(modRM._Mod = 3, ReadRegister32(modRM.RM), Mem.ReadDWord(DecodeEffectiveAddress(modRM)))
                    Dim r32 As UInteger = ReadRegister32(modRM.Reg)
                    Dim result As ULong = CULng(rm32) - CULng(r32) - CULng(If(CarryFlag, 1, 0))
                    CarryFlag = result > UInteger.MaxValue
                    If modRM._Mod = 3 Then
                        WriteRegister32(modRM.RM, CUInt(result And &HFFFFFFFFUI))
                    Else
                        Mem.WriteDWord(DecodeEffectiveAddress(modRM), CUInt(result And &HFFFFFFFFUI))
                    End If
                    SetFlagsAfterOperation(CUInt(result And &HFFFFFFFFUI), 32)
                End If

            Case &H1A ' SBB r8, r/m8
                Dim r8 As Byte = ReadRegister8(modRM.Reg)
                Dim rm8 As Byte = If(modRM._Mod = 3, ReadRegister8(modRM.RM), Mem.ReadByte(DecodeEffectiveAddress(modRM)))
                Dim result As UShort = CUShort(r8) - CUShort(rm8) - CUShort(If(CarryFlag, 1, 0))
                CarryFlag = result > &HFF
                WriteRegister8(modRM.Reg, CByte(result And &HFF))
                SetFlagsAfterOperation(CByte(result And &HFF), 8)

            Case &H1B ' SBB r16, r/m16 or SBB r32, r/m32
                If prefixes.OperandSizeOverride Then
                    Dim r16 As UShort = ReadRegister16(modRM.Reg)
                    Dim rm16 As UShort = If(modRM._Mod = 3, ReadRegister16(modRM.RM), Mem.ReadWord(DecodeEffectiveAddress(modRM)))
                    Dim result As UInteger = CUInt(r16) - CUInt(rm16) - CUInt(If(CarryFlag, 1, 0))
                    CarryFlag = result > &HFFFF
                    WriteRegister16(modRM.Reg, CUShort(result And &HFFFF))
                    SetFlagsAfterOperation(CUShort(result And &HFFFF), 16)
                Else
                    Dim r32 As UInteger = ReadRegister32(modRM.Reg)
                    Dim rm32 As UInteger = If(modRM._Mod = 3, ReadRegister32(modRM.RM), Mem.ReadDWord(DecodeEffectiveAddress(modRM)))
                    Dim result As ULong = CULng(r32) - CULng(rm32) - CULng(If(CarryFlag, 1, 0))
                    CarryFlag = result > UInteger.MaxValue
                    WriteRegister32(modRM.Reg, CUInt(result And &HFFFFFFFFUI))
                    SetFlagsAfterOperation(CUInt(result And &HFFFFFFFFUI), 32)
                End If

            Case &H1C ' SBB AL, imm8
                Dim imm8 As Byte = Mem.ReadByte(EIP)
                EIP += 1
                Dim result As UShort = CUShort(AL) - CUShort(imm8) - CUShort(If(CarryFlag, 1, 0))
                CarryFlag = result > &HFF
                AL = CByte(result And &HFF)
                SetFlagsAfterOperation(AL, 8)

            Case &H1D ' SBB AX, imm16 or SBB EAX, imm32
                If prefixes.OperandSizeOverride Then
                    Dim imm16 As UShort = Mem.ReadWord(EIP)
                    EIP += 2
                    Dim result As UInteger = CUInt(AX) - CUInt(imm16) - CUInt(If(CarryFlag, 1, 0))
                    CarryFlag = result > &HFFFF
                    AX = CUShort(result And &HFFFF)
                    SetFlagsAfterOperation(AX, 16)
                Else
                    Dim imm32 As UInteger = Mem.ReadDWord(EIP)
                    EIP += 4
                    Dim result As ULong = CULng(EAX) - CULng(imm32) - CULng(If(CarryFlag, 1, 0))
                    CarryFlag = result > UInteger.MaxValue
                    EAX = CUInt(result And &HFFFFFFFFUI)
                    SetFlagsAfterOperation(EAX, 32)
                End If

            Case Else
                LogError("Opcode SBB non supportato: " & opcode.ToString("X2"))
        End Select
    End Sub

    Private Sub ExecuteIMUL(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()
        Dim size As Integer = If(prefixes.OperandSizeOverride, 16, 32)

        Select Case opcode
            Case &H6B ' IMUL r16/32, r/m16/32, imm8
                Dim dest As Integer
                Dim source As Integer
                Dim imm8 As SByte = CSByte(Mem.ReadByte(EIP))
                EIP += 1

                If modRM._Mod = 3 Then
                    source = If(size = 16, CShort(ReadRegister16(modRM.RM)), CInt(ReadRegister32(modRM.RM)))
                Else
                    Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                    source = If(size = 16, CShort(Mem.ReadWord(effectiveAddress)), CInt(Mem.ReadDWord(effectiveAddress)))
                End If

                dest = source * CInt(imm8)

                If size = 16 Then
                    WriteRegister16(modRM.Reg, CUShort(dest And &HFFFF))
                    SetFlagsAfterIMUL(dest, &HFFFF)
                Else
                    WriteRegister32(modRM.Reg, CUInt(dest))
                    SetFlagsAfterIMUL(dest, &HFFFFFFFFL)
                End If

            Case &H69 ' IMUL r16/32, r/m16/32, imm16/32
                Dim dest As Integer
                Dim source As Integer
                Dim imm As Integer

                If modRM._Mod = 3 Then
                    source = If(size = 16, CShort(ReadRegister16(modRM.RM)), CInt(ReadRegister32(modRM.RM)))
                Else
                    Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                    source = If(size = 16, CShort(Mem.ReadWord(effectiveAddress)), CInt(Mem.ReadDWord(effectiveAddress)))
                End If

                If size = 16 Then
                    imm = CShort(Mem.ReadWord(EIP))
                    EIP += 2
                Else
                    imm = CInt(Mem.ReadDWord(EIP))
                    EIP += 4
                End If

                dest = source * imm

                If size = 16 Then
                    WriteRegister16(modRM.Reg, CUShort(dest And &HFFFF))
                    SetFlagsAfterIMUL(dest, &HFFFF)
                Else
                    WriteRegister32(modRM.Reg, CUInt(dest))
                    SetFlagsAfterIMUL(dest, &HFFFFFFFFL)
                End If

            Case &HAF ' IMUL r16/32, r/m16/32
                Dim dest As Integer
                Dim source As Integer

                If size = 16 Then
                    dest = CShort(ReadRegister16(modRM.Reg))
                    If modRM._Mod = 3 Then
                        source = CShort(ReadRegister16(modRM.RM))
                    Else
                        Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                        source = CShort(Mem.ReadWord(effectiveAddress))
                    End If
                Else
                    dest = CInt(ReadRegister32(modRM.Reg))
                    If modRM._Mod = 3 Then
                        source = CInt(ReadRegister32(modRM.RM))
                    Else
                        Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                        source = CInt(Mem.ReadDWord(effectiveAddress))
                    End If
                End If

                dest *= source

                If size = 16 Then
                    WriteRegister16(modRM.Reg, CUShort(dest And &HFFFF))
                    SetFlagsAfterIMUL(dest, &HFFFF)
                Else
                    WriteRegister32(modRM.Reg, CUInt(dest))
                    SetFlagsAfterIMUL(dest, &HFFFFFFFFL)
                End If

            Case &HF6, &HF7 ' IMUL r/m8 or IMUL r/m16/32
                size = If(opcode = &HF6, 8, If(prefixes.OperandSizeOverride, 16, 32))
                Dim value As Integer

                If modRM._Mod = 3 Then
                    value = If(size = 8, CSByte(ReadRegister8(modRM.RM)),
                               If(size = 16, CShort(ReadRegister16(modRM.RM)),
                                  CInt(ReadRegister32(modRM.RM))))
                Else
                    Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
                    value = If(size = 8, CSByte(Mem.ReadByte(effectiveAddress)),
                               If(size = 16, CShort(Mem.ReadWord(effectiveAddress)),
                                  CInt(Mem.ReadDWord(effectiveAddress))))
                End If

                Select Case size
                    Case 8
                        Dim result As Short = CShort(AL) * CShort(value)
                        AX = CUShort(result)
                        SetFlagsAfterIMUL(result, &HFF)
                    Case 16
                        Dim result As Integer = CInt(AX) * value
                        DX = CUShort((result >> 16) And &HFFFF)
                        AX = CUShort(result And &HFFFF)
                        SetFlagsAfterIMUL(result, &HFFFF)
                    Case 32
                        Dim result As Long = CLng(EAX) * CLng(value)
                        EDX = CUInt((result >> 32) And &HFFFFFFFFL)
                        EAX = CUInt(result And &HFFFFFFFFL)
                        SetFlagsAfterIMUL(result, &HFFFFFFFFL)
                End Select

            Case Else
                LogError("Opcode IMUL non supportato: " & opcode.ToString("X2"))
        End Select
    End Sub

    Private Sub ExecuteMUL(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()
        Dim size As Integer = If(opcode = &HF6, 8, If(prefixes.OperandSizeOverride, 16, 32))
        Dim value As UInteger

        If modRM._Mod = 3 Then
            value = If(size = 8, ReadRegister8(modRM.RM),
                       If(size = 16, ReadRegister16(modRM.RM),
                          ReadRegister32(modRM.RM)))
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            value = If(size = 8, Mem.ReadByte(effectiveAddress),
                       If(size = 16, Mem.ReadWord(effectiveAddress),
                          Mem.ReadDWord(effectiveAddress)))
        End If

        Select Case size
            Case 8
                Dim result As UShort = CUShort(AL) * CUShort(value)
                AX = result
                CarryFlag = (result > &HFF)
                OverflowFlag = CarryFlag
            Case 16
                Dim result As UInteger = CUInt(AX) * CUInt(value)
                DX = CUShort((result >> 16) And &HFFFF)
                AX = CUShort(result And &HFFFF)
                CarryFlag = (result > &HFFFF)
                OverflowFlag = CarryFlag
            Case 32
                Dim result As ULong = CULng(EAX) * CULng(value)
                EDX = CUInt((result >> 32) And &HFFFFFFFFL)
                EAX = CUInt(result And &HFFFFFFFFL)
                CarryFlag = (result > UInteger.MaxValue)
                OverflowFlag = CarryFlag
        End Select

        ' MUL influisce solo su CF e OF, gli altri flag sono indefiniti
    End Sub

    Private Sub ExecuteDIV(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()
        Dim size As Integer = If(opcode = &HF6, 8, If(prefixes.OperandSizeOverride, 16, 32))
        Dim divisor As UInteger

        If modRM._Mod = 3 Then
            divisor = If(size = 8, ReadRegister8(modRM.RM),
                         If(size = 16, ReadRegister16(modRM.RM),
                            ReadRegister32(modRM.RM)))
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            divisor = If(size = 8, Mem.ReadByte(effectiveAddress),
                         If(size = 16, Mem.ReadWord(effectiveAddress),
                            Mem.ReadDWord(effectiveAddress)))
        End If

        If divisor = 0 Then
            ' Genera un'eccezione di divisione per zero
            Throw New DivideByZeroException("Divisione per zero in DIV")
        End If

        Select Case size
            Case 8
                Dim dividend As UShort = AX
                Dim quotient As Byte = CByte(dividend \ divisor)
                Dim remainder As Byte = CByte(dividend Mod divisor)
                If quotient > &HFF Then
                    ' Genera un'eccezione di overflow
                    Throw New OverflowException("Overflow in DIV")
                End If
                AL = quotient
                AH = remainder
            Case 16
                Dim dividend As UInteger = (CUInt(DX) << 16) Or AX
                Dim quotient As UShort = CUShort(dividend \ divisor)
                Dim remainder As UShort = CUShort(dividend Mod divisor)
                If (dividend \ divisor) > &HFFFF Then
                    ' Genera un'eccezione di overflow
                    Throw New OverflowException("Overflow in DIV")
                End If
                AX = quotient
                DX = remainder
            Case 32
                Dim dividend As ULong = (CULng(EDX) << 32) Or EAX
                Dim quotient As UInteger = CUInt(dividend \ divisor)
                Dim remainder As UInteger = CUInt(dividend Mod divisor)
                If (dividend \ divisor) > UInteger.MaxValue Then
                    ' Genera un'eccezione di overflow
                    Throw New OverflowException("Overflow in DIV")
                End If
                EAX = quotient
                EDX = remainder
        End Select

        ' DIV non influisce sui flag
    End Sub

    Private Sub ExecuteIDIV(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()
        Dim size As Integer = If(opcode = &HF6, 8, If(prefixes.OperandSizeOverride, 16, 32))
        Dim divisor As Integer

        If modRM._Mod = 3 Then
            divisor = If(size = 8, CSByte(ReadRegister8(modRM.RM)),
                         If(size = 16, CShort(ReadRegister16(modRM.RM)),
                            CInt(ReadRegister32(modRM.RM))))
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            divisor = If(size = 8, CSByte(Mem.ReadByte(effectiveAddress)),
                         If(size = 16, CShort(Mem.ReadWord(effectiveAddress)),
                            CInt(Mem.ReadDWord(effectiveAddress))))
        End If

        If divisor = 0 Then
            ' Genera un'eccezione di divisione per zero
            Throw New DivideByZeroException("Divisione per zero in IDIV")
        End If

        Select Case size
            Case 8
                Dim dividend As Short = CShort(AX)
                Dim quotient As SByte = CSByte(dividend \ divisor)
                Dim remainder As SByte = CSByte(dividend Mod divisor)
                If quotient < SByte.MinValue OrElse quotient > SByte.MaxValue Then
                    ' Genera un'eccezione di overflow
                    Throw New OverflowException("Overflow in IDIV")
                End If
                AL = CByte(quotient)
                AH = CByte(remainder)
            Case 16
                Dim dividend As Integer = (CInt(DX) << 16) Or AX
                Dim quotient As Short = CShort(dividend \ divisor)
                Dim remainder As Short = CShort(dividend Mod divisor)
                If quotient < Short.MinValue OrElse quotient > Short.MaxValue Then
                    ' Genera un'eccezione di overflow
                    Throw New OverflowException("Overflow in IDIV")
                End If
                AX = CUShort(quotient)
                DX = CUShort(remainder)
            Case 32
                Dim dividend As Long = (CLng(EDX) << 32) Or EAX
                Dim quotient As Integer = CInt(dividend \ divisor)
                Dim remainder As Integer = CInt(dividend Mod divisor)
                If quotient < Integer.MinValue OrElse quotient > Integer.MaxValue Then
                    ' Genera un'eccezione di overflow
                    Throw New OverflowException("Overflow in IDIV")
                End If
                EAX = CUInt(quotient)
                EDX = CUInt(remainder)
        End Select

        ' IDIV non influisce sui flag
    End Sub

    Private Sub ExecuteXADD(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()
        Dim size As Integer = If(opcode = &HC0, 8, If(prefixes.OperandSizeOverride, 16, 32))

        Dim value1, value2 As UInteger

        If modRM._Mod = 3 Then
            value1 = If(size = 8, ReadRegister8(modRM.RM),
                        If(size = 16, ReadRegister16(modRM.RM),
                           ReadRegister32(modRM.RM)))
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            value1 = If(size = 8, Mem.ReadByte(effectiveAddress),
                        If(size = 16, Mem.ReadWord(effectiveAddress),
                           Mem.ReadDWord(effectiveAddress)))
        End If

        value2 = If(size = 8, ReadRegister8(modRM.Reg),
                    If(size = 16, ReadRegister16(modRM.Reg),
                       ReadRegister32(modRM.Reg)))

        Dim sum As ULong = CULng(value1) + CULng(value2)

        ' Scrive il risultato nella destinazione
        If modRM._Mod = 3 Then
            Select Case size
                Case 8
                    WriteRegister8(modRM.RM, CByte(sum And &HFF))
                Case 16
                    WriteRegister16(modRM.RM, CUShort(sum And &HFFFF))
                Case 32
                    WriteRegister32(modRM.RM, CUInt(sum And &HFFFFFFFFUI))
            End Select
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            Select Case size
                Case 8
                    Mem.WriteByte(effectiveAddress, CByte(sum And &HFF))
                Case 16
                    Mem.WriteWord(effectiveAddress, CUShort(sum And &HFFFF))
                Case 32
                    Mem.WriteDWord(effectiveAddress, CUInt(sum And &HFFFFFFFFUI))
            End Select
        End If

        ' Scrive il valore originale della destinazione nel registro source
        Select Case size
            Case 8
                WriteRegister8(modRM.Reg, CByte(value1))
            Case 16
                WriteRegister16(modRM.Reg, CUShort(value1))
            Case 32
                WriteRegister32(modRM.Reg, CUInt(value1))
        End Select

        ' Imposta i flag
        SetFlagsAfterOperation(CUInt(sum And &HFFFFFFFFUI), size)
    End Sub

    Private Sub ExecuteSUB(opcode As Byte)
        Dim modRM As ModRMByte = DecodeModRM()

        Select Case opcode
            Case &H28 ' SUB r/m8, r8
                Dim rm8 As Byte = If(modRM._Mod = 3, ReadRegister8(modRM.RM), Mem.ReadByte(DecodeEffectiveAddress(modRM)))
                Dim r8 As Byte = ReadRegister8(modRM.Reg)
                Dim result As UShort = CUShort(rm8) - CUShort(r8)
                CarryFlag = result > &HFF
                If modRM._Mod = 3 Then
                    WriteRegister8(modRM.RM, CByte(result And &HFF))
                Else
                    Mem.WriteByte(DecodeEffectiveAddress(modRM), CByte(result And &HFF))
                End If
                SetFlagsAfterOperation(CByte(result And &HFF), 8)

            Case &H29 ' SUB r/m16, r16 or SUB r/m32, r32
                If prefixes.OperandSizeOverride Then
                    Dim rm16 As UShort = If(modRM._Mod = 3, ReadRegister16(modRM.RM), Mem.ReadWord(DecodeEffectiveAddress(modRM)))
                    Dim r16 As UShort = ReadRegister16(modRM.Reg)
                    Dim result As UInteger = CUInt(rm16) - CUInt(r16)
                    CarryFlag = result > &HFFFF
                    If modRM._Mod = 3 Then
                        WriteRegister16(modRM.RM, CUShort(result And &HFFFF))
                    Else
                        Mem.WriteWord(DecodeEffectiveAddress(modRM), CUShort(result And &HFFFF))
                    End If
                    SetFlagsAfterOperation(CUShort(result And &HFFFF), 16)
                Else
                    Dim rm32 As UInteger = If(modRM._Mod = 3, ReadRegister32(modRM.RM), Mem.ReadDWord(DecodeEffectiveAddress(modRM)))
                    Dim r32 As UInteger = ReadRegister32(modRM.Reg)
                    Dim result As ULong = CULng(rm32) - CULng(r32)
                    CarryFlag = result > UInteger.MaxValue
                    If modRM._Mod = 3 Then
                        WriteRegister32(modRM.RM, CUInt(result And &HFFFFFFFFUI))
                    Else
                        Mem.WriteDWord(DecodeEffectiveAddress(modRM), CUInt(result And &HFFFFFFFFUI))
                    End If
                    SetFlagsAfterOperation(CUInt(result And &HFFFFFFFFUI), 32)
                End If

            Case &H2A ' SUB r8, r/m8
                Dim r8 As Byte = ReadRegister8(modRM.Reg)
                Dim rm8 As Byte = If(modRM._Mod = 3, ReadRegister8(modRM.RM), Mem.ReadByte(DecodeEffectiveAddress(modRM)))
                Dim result As UShort = CUShort(r8) - CUShort(rm8)
                CarryFlag = result > &HFF
                WriteRegister8(modRM.Reg, CByte(result And &HFF))
                SetFlagsAfterOperation(CByte(result And &HFF), 8)

            Case &H2B ' SUB r16, r/m16 or SUB r32, r/m32
                If prefixes.OperandSizeOverride Then
                    Dim r16 As UShort = ReadRegister16(modRM.Reg)
                    Dim rm16 As UShort = If(modRM._Mod = 3, ReadRegister16(modRM.RM), Mem.ReadWord(DecodeEffectiveAddress(modRM)))
                    Dim result As UInteger = CUInt(r16) - CUInt(rm16)
                    CarryFlag = result > &HFFFF
                    WriteRegister16(modRM.Reg, CUShort(result And &HFFFF))
                    SetFlagsAfterOperation(CUShort(result And &HFFFF), 16)
                Else
                    Dim r32 As UInteger = ReadRegister32(modRM.Reg)
                    Dim rm32 As UInteger = If(modRM._Mod = 3, ReadRegister32(modRM.RM), Mem.ReadDWord(DecodeEffectiveAddress(modRM)))
                    Dim result As ULong = CULng(r32) - CULng(rm32)
                    CarryFlag = result > UInteger.MaxValue
                    WriteRegister32(modRM.Reg, CUInt(result And &HFFFFFFFFUI))
                    SetFlagsAfterOperation(CUInt(result And &HFFFFFFFFUI), 32)
                End If

            Case &H2C ' SUB AL, imm8
                Dim imm8 As Byte = Mem.ReadByte(EIP)
                EIP += 1
                Dim result As UShort = CUShort(AL) - CUShort(imm8)
                CarryFlag = result > &HFF
                AL = CByte(result And &HFF)
                SetFlagsAfterOperation(AL, 8)

            Case &H2D ' SUB AX, imm16 or SUB EAX, imm32
                If prefixes.OperandSizeOverride Then
                    Dim imm16 As UShort = Mem.ReadWord(EIP)
                    EIP += 2
                    Dim result As UInteger = CUInt(AX) - CUInt(imm16)
                    CarryFlag = result > &HFFFF
                    AX = CUShort(result And &HFFFF)
                    SetFlagsAfterOperation(AX, 16)
                Else
                    Dim imm32 As UInteger = Mem.ReadDWord(EIP)
                    EIP += 4
                    Dim result As ULong = CULng(EAX) - CULng(imm32)
                    CarryFlag = result > UInteger.MaxValue
                    EAX = CUInt(result And &HFFFFFFFFUI)
                    SetFlagsAfterOperation(EAX, 32)
                End If

            Case &H80, &H82 ' SUB r/m8, imm8
                Dim rm8 As Byte = If(modRM._Mod = 3, ReadRegister8(modRM.RM), Mem.ReadByte(DecodeEffectiveAddress(modRM)))
                Dim imm8 As Byte = Mem.ReadByte(EIP)
                EIP += 1
                Dim result As UShort = CUShort(rm8) - CUShort(imm8)
                CarryFlag = result > &HFF
                If modRM._Mod = 3 Then
                    WriteRegister8(modRM.RM, CByte(result And &HFF))
                Else
                    Mem.WriteByte(DecodeEffectiveAddress(modRM), CByte(result And &HFF))
                End If
                SetFlagsAfterOperation(CByte(result And &HFF), 8)

            Case &H81 ' SUB r/m16, imm16 or SUB r/m32, imm32
                If prefixes.OperandSizeOverride Then
                    Dim rm16 As UShort = If(modRM._Mod = 3, ReadRegister16(modRM.RM), Mem.ReadWord(DecodeEffectiveAddress(modRM)))
                    Dim imm16 As UShort = Mem.ReadWord(EIP)
                    EIP += 2
                    Dim result As UInteger = CUInt(rm16) - CUInt(imm16)
                    CarryFlag = result > &HFFFF
                    If modRM._Mod = 3 Then
                        WriteRegister16(modRM.RM, CUShort(result And &HFFFF))
                    Else
                        Mem.WriteWord(DecodeEffectiveAddress(modRM), CUShort(result And &HFFFF))
                    End If
                    SetFlagsAfterOperation(CUShort(result And &HFFFF), 16)
                Else
                    Dim rm32 As UInteger = If(modRM._Mod = 3, ReadRegister32(modRM.RM), Mem.ReadDWord(DecodeEffectiveAddress(modRM)))
                    Dim imm32 As UInteger = Mem.ReadDWord(EIP)
                    EIP += 4
                    Dim result As ULong = CULng(rm32) - CULng(imm32)
                    CarryFlag = result > UInteger.MaxValue
                    If modRM._Mod = 3 Then
                        WriteRegister32(modRM.RM, CUInt(result And &HFFFFFFFFUI))
                    Else
                        Mem.WriteDWord(DecodeEffectiveAddress(modRM), CUInt(result And &HFFFFFFFFUI))
                    End If
                    SetFlagsAfterOperation(CUInt(result And &HFFFFFFFFUI), 32)
                End If

            Case &H83 ' SUB r/m16, imm8 (sign-extended) or SUB r/m32, imm8 (sign-extended)
                If prefixes.OperandSizeOverride Then
                    Dim rm16 As UShort = If(modRM._Mod = 3, ReadRegister16(modRM.RM), Mem.ReadWord(DecodeEffectiveAddress(modRM)))
                    Dim imm8 As SByte = CSByte(Mem.ReadByte(EIP))
                    EIP += 1
                    Dim result As Integer = CInt(rm16) - CInt(imm8)
                    CarryFlag = result < 0 OrElse result > &HFFFF
                    If modRM._Mod = 3 Then
                        WriteRegister16(modRM.RM, CUShort(result And &HFFFF))
                    Else
                        Mem.WriteWord(DecodeEffectiveAddress(modRM), CUShort(result And &HFFFF))
                    End If
                    SetFlagsAfterOperation(CUShort(result And &HFFFF), 16)
                Else
                    Dim rm32 As UInteger = If(modRM._Mod = 3, ReadRegister32(modRM.RM), Mem.ReadDWord(DecodeEffectiveAddress(modRM)))
                    Dim imm8 As SByte = CSByte(Mem.ReadByte(EIP))
                    EIP += 1
                    Dim result As Long = CLng(rm32) - CLng(imm8)
                    CarryFlag = result < 0 OrElse result > UInteger.MaxValue
                    If modRM._Mod = 3 Then
                        WriteRegister32(modRM.RM, CUInt(result And &HFFFFFFFFL))
                    Else
                        Mem.WriteDWord(DecodeEffectiveAddress(modRM), CUInt(result And &HFFFFFFFFL))
                    End If
                    SetFlagsAfterOperation(CUInt(result And &HFFFFFFFFL), 32)
                End If

            Case Else
                LogError("Opcode SUB non supportato: " & opcode.ToString("X2"))
        End Select
    End Sub

    Private Sub ExecuteINS(opcode As Byte)
        Dim size As Integer = If(opcode = &H6C, 8, If(prefixes.OperandSizeOverride, 16, 32))

        If prefixes.RepeatPrefix <> 0 Then
            ' REPS INS
            While ECX <> 0
                Select Case size
                    Case 8
                        Mem.WriteByte(EDI, InputByte(DX))
                    Case 16
                        Mem.WriteWord(EDI, InputWord(DX))
                    Case 32
                        Mem.WriteDWord(EDI, InputDWord(DX))
                End Select

                If DirectionFlag Then
                    EDI -= CUInt(size \ 8)
                Else
                    EDI += CUInt(size \ 8)
                End If
                ECX -= 1
                If InterruptFlag Then Exit While ' Controllo interruzioni
            End While
        Else
            ' INS singola
            Select Case size
                Case 8
                    Mem.WriteByte(EDI, InputByte(DX))
                Case 16
                    Mem.WriteWord(EDI, InputWord(DX))
                Case 32
                    Mem.WriteDWord(EDI, InputDWord(DX))
            End Select

            If DirectionFlag Then
                EDI -= CUInt(size \ 8)
            Else
                EDI += CUInt(size \ 8)
            End If
        End If
    End Sub

    Private Sub ExecuteOUTS(opcode As Byte)
        Dim size As Integer = If(opcode = &H6E, 8, If(prefixes.OperandSizeOverride, 16, 32))

        If prefixes.RepeatPrefix <> 0 Then
            ' REPS OUTS
            While ECX <> 0
                Select Case size
                    Case 8
                        OutputByte(DX, Mem.ReadByte(ESI))
                    Case 16
                        OutputWord(DX, Mem.ReadWord(ESI))
                    Case 32
                        OutputDWord(DX, Mem.ReadDWord(ESI))
                End Select

                If DirectionFlag Then
                    ESI -= CUInt(size \ 8)
                Else
                    ESI += CUInt(size \ 8)
                End If
                ECX -= 1
                If InterruptFlag Then Exit While ' Controllo interruzioni
            End While
        Else
            ' OUTS singola
            Select Case size
                Case 8
                    OutputByte(DX, Mem.ReadByte(ESI))
                Case 16
                    OutputWord(DX, Mem.ReadWord(ESI))
                Case 32
                    OutputDWord(DX, Mem.ReadDWord(ESI))
            End Select

            If DirectionFlag Then
                ESI -= CUInt(size \ 8)
            Else
                ESI += CUInt(size \ 8)
            End If
        End If
    End Sub
    Private Sub ExecuteMOVS(opcode As Byte)
        Dim size As Integer = If(opcode = &HA4, 8, If(prefixes.OperandSizeOverride, 16, 32))

        If prefixes.RepeatPrefix <> 0 Then
            ' REP MOVS
            While ECX <> 0
                Select Case size
                    Case 8
                        Mem.WriteByte(EDI, Mem.ReadByte(ESI))
                    Case 16
                        Mem.WriteWord(EDI, Mem.ReadWord(ESI))
                    Case 32
                        Mem.WriteDWord(EDI, Mem.ReadDWord(ESI))
                End Select

                If DirectionFlag Then
                    ESI -= CUInt(size \ 8)
                    EDI -= CUInt(size \ 8)
                Else
                    ESI += CUInt(size \ 8)
                    EDI += CUInt(size \ 8)
                End If
                ECX -= 1
                If InterruptFlag Then Exit While ' Controllo interruzioni
            End While
        Else
            ' MOVS singola
            Select Case size
                Case 8
                    Mem.WriteByte(EDI, Mem.ReadByte(ESI))
                Case 16
                    Mem.WriteWord(EDI, Mem.ReadWord(ESI))
                Case 32
                    Mem.WriteDWord(EDI, Mem.ReadDWord(ESI))
            End Select

            If DirectionFlag Then
                ESI -= CUInt(size \ 8)
                EDI -= CUInt(size \ 8)
            Else
                ESI += CUInt(size \ 8)
                EDI += CUInt(size \ 8)
            End If
        End If
    End Sub

    Private Sub ExecuteIN(opcode As Byte)
        Select Case opcode
            Case &HE4 ' IN AL, imm8
                Dim port As Byte = Mem.ReadByte(EIP)
                EIP += 1
                AL = InputByte(port)

            Case &HE5 ' IN AX/EAX, imm8
                Dim port As Byte = Mem.ReadByte(EIP)
                EIP += 1
                If prefixes.OperandSizeOverride Then
                    AX = InputWord(port)
                Else
                    EAX = InputDWord(port)
                End If

            Case &HEC ' IN AL, DX
                AL = InputByte(DX)

            Case &HED ' IN AX/EAX, DX
                If prefixes.OperandSizeOverride Then
                    AX = InputWord(DX)
                Else
                    EAX = InputDWord(DX)
                End If

            Case Else
                LogError("Opcode IN non valido: " & opcode.ToString("X2"))
        End Select
    End Sub

    Private Sub ExecuteOUT(opcode As Byte)
        Select Case opcode
            Case &HE6 ' OUT imm8, AL
                Dim port As Byte = Mem.ReadByte(EIP)
                EIP += 1
                OutputByte(port, AL)

            Case &HE7 ' OUT imm8, AX/EAX
                Dim port As Byte = Mem.ReadByte(EIP)
                EIP += 1
                If prefixes.OperandSizeOverride Then
                    OutputWord(port, AX)
                Else
                    OutputDWord(port, EAX)
                End If

            Case &HEE ' OUT DX, AL
                OutputByte(DX, AL)

            Case &HEF ' OUT DX, AX/EAX
                If prefixes.OperandSizeOverride Then
                    OutputWord(DX, AX)
                Else
                    OutputDWord(DX, EAX)
                End If

            Case Else
                LogError("Opcode OUT non valido: " & opcode.ToString("X2"))
        End Select
    End Sub

    Private Function InputByte(port As Object) As Byte
        RaiseEvent IngRQ(Me, port, 1)
        Return ValoreI_O
    End Function

    Private Function InputWord(port As Object) As UShort
        RaiseEvent IngRQ(Me, port, 2)
        Return ValoreI_O
    End Function

    Private Function InputDWord(port As Object) As UInteger
        RaiseEvent IngRQ(Me, port, 4)
        Return ValoreI_O
    End Function

    Private Sub OutputByte(port As Object, Value As Byte)
        ValoreI_O = Value
        RaiseEvent OutRQ(Me, port, 1)
    End Sub

    Private Sub OutputWord(port As Object, Value As UShort)
        ValoreI_O = Value
        RaiseEvent OutRQ(Me, port, 2)
    End Sub

    Private Sub OutputDWord(port As Object, Value As UInteger)
        ValoreI_O = Value
        RaiseEvent OutRQ(Me, port, 4)
    End Sub

    Private Sub ExecuteFADD()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim operand As Double

        If modRM._Mod = 3 Then
            ' ST(0) <- ST(0) + ST(i)
            operand = ReadSTRegister(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            Select Case modRM.Reg
                Case 0 ' FADD single-precision
                    operand = CSng(Mem.ReadSingle(effectiveAddress))
                Case 2 ' FADD double-precision
                    operand = Mem.ReadQWord(effectiveAddress)
                Case Else
                    LogError("Operando FADD non valido")
            End Select
        End If

        Dim result As Double = ST0 + operand
        ST0 = result
        SetFPUFlags(result)
    End Sub

    Private Sub ExecuteFMUL()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim operand As Double

        If modRM._Mod = 3 Then
            ' ST(0) <- ST(0) * ST(i)
            operand = ReadSTRegister(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            Select Case modRM.Reg
                Case 1 ' FMUL single-precision
                    operand = CSng(Mem.ReadSingle(effectiveAddress))
                Case 3 ' FMUL double-precision
                    operand = Mem.ReadQWord(effectiveAddress)
                Case Else
                    LogError("Operando FMUL non valido")
            End Select
        End If

        Dim result As Double = ST0 * operand
        ST0 = result
        SetFPUFlags(result)
    End Sub

    Private Sub ExecuteFCOM()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim operand As Double

        If modRM._Mod = 3 Then
            ' Compare ST(0) with ST(i)
            operand = ReadSTRegister(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            Select Case modRM.Reg
                Case 2 ' FCOM single-precision
                    operand = CSng(Mem.ReadSingle(effectiveAddress))
                Case 4 ' FCOM double-precision
                    operand = Mem.ReadQWord(effectiveAddress)
                Case Else
                    LogError("Operando FCOM non valido")
            End Select
        End If

        If Double.IsNaN(ST0) OrElse Double.IsNaN(operand) Then
            SetFPUFlags(Double.NaN)
        Else
            SetFPUFlags(ST0 - operand)
        End If
    End Sub

    Private Sub ExecuteFSUB()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim operand As Double

        If modRM._Mod = 3 Then
            ' ST(0) <- ST(0) - ST(i)
            operand = ReadSTRegister(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            Select Case modRM.Reg
                Case 4 ' FSUB single-precision
                    operand = CSng(Mem.ReadSingle(effectiveAddress))
                Case 6 ' FSUB double-precision
                    operand = Mem.ReadQWord(effectiveAddress)
                Case Else
                    LogError("Operando FSUB non valido")
            End Select
        End If
        Dim result As Double = ST0 - operand
        ST0 = result
        SetFPUFlags(result)
    End Sub

    Private Sub ExecuteFDIV()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim operand As Double

        If modRM._Mod = 3 Then
            ' ST(0) <- ST(0) / ST(i)
            operand = ReadSTRegister(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            Select Case modRM.Reg
                Case 6 ' FDIV single-precision
                    operand = CSng(Mem.ReadSingle(effectiveAddress))
                Case 8 ' FDIV double-precision
                    operand = Mem.ReadQWord(effectiveAddress)
                Case Else
                    LogError("Operando FDIV non valido")
            End Select
        End If

        If operand = 0 Then
            ' Gestione della divisione per zero
            ' Qui potresti voler impostare un flag di eccezione FPU o gestire in altro modo
            Throw New DivideByZeroException("Divisione per zero in FDIV")
        Else
            Dim result As Double = ST0 / operand
            ST0 = result
            SetFPUFlags(result)
        End If
    End Sub

    Private Sub ExecuteFLD()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim value As Double

        If modRM._Mod = 3 Then
            ' Carica da un altro registro ST
            value = ReadSTRegister(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            Select Case modRM.Reg
                Case 0 ' FLD single-precision (32-bit)
                    value = CSng(Mem.ReadSingle(effectiveAddress))
                Case 2 ' FLD double-precision (64-bit)
                    value = Mem.ReadQWord(effectiveAddress)
                Case 5 ' FLD extended-precision (80-bit)
                    Dim extendedBytes As Byte() = Mem.Read80Bit(effectiveAddress)
                    value = ConvertExtendedToDouble(extendedBytes)
                Case Else
                    LogError("Operando FLD non valido")
            End Select
        End If

        PushFPUStack(value)
    End Sub

    Private Sub ExecuteFST()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim value As Double = ST0

        If modRM._Mod = 3 Then
            ' Salva in un altro registro ST
            WriteSTRegister(modRM.RM, value)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            Select Case modRM.Reg
                Case 2 ' FST single-precision (32-bit)
                    Mem.WriteSingle(effectiveAddress, CSng(value))
                Case 4 ' FST double-precision (64-bit)
                    Mem.WriteQWord(effectiveAddress, value)
                Case Else
                    LogError("Operando FST non valido")
            End Select
        End If
    End Sub

    Private Sub ExecuteFSTP()
        ExecuteFST() ' Esegue FST
        PopFPUStack() ' Poi rimuove il valore dallo stack
    End Sub

    Private Function ConvertExtendedToDouble(extendedBytes As Byte()) As Double
        ' Implementazione semplificata della conversione da 80-bit extended precision a double
        ' Nota: questa è una versione molto semplificata e potrebbe non gestire tutti i casi
        Dim sign As Integer = If((extendedBytes(9) And &H80) <> 0, -1, 1)
        Dim exponent As Integer = (CInt(extendedBytes(9) And &H7F) << 8) Or extendedBytes(8)
        Dim mantissa As ULong = BitConverter.ToUInt64(extendedBytes, 0)

        If exponent = 0 And mantissa = 0 Then
            Return 0
        ElseIf exponent = &H7FFF Then
            If mantissa = &H8000000000000000UL Then
                Return sign * Double.PositiveInfinity
            Else
                Return Double.NaN
            End If
        End If

        exponent -= 16383 ' Bias
        Dim result As Double = sign * Math.Pow(2, exponent) * (1 + mantissa / (2.0 ^ 63))
        Return result
    End Function

    Private Sub ExecuteFXCH()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim stIndex As Byte = If(modRM._Mod = 3, modRM.RM, 1) ' Se non è specificato, assume ST(1)

        Dim temp As Double = ReadSTRegister(0) ' ST(0)
        WriteSTRegister(0, ReadSTRegister(stIndex))
        WriteSTRegister(stIndex, temp)
    End Sub

    Private Sub ExecuteFSQRT()
        Dim st0 As Double = ReadSTRegister(0)
        If st0 < 0 Then
            ' Impostare il flag di eccezione Invalid Operation
            WriteSTRegister(0, Double.NaN)
        Else
            WriteSTRegister(0, Math.Sqrt(st0))
        End If
        SetFPUFlags(ReadSTRegister(0))
    End Sub

    Private Sub ExecuteFSCALE()
        Dim st0 As Double = ReadSTRegister(0)
        Dim st1 As Double = ReadSTRegister(1)
        Dim scale As Integer = CInt(Math.Truncate(st1))

        Dim result As Double = st0 * Math.Pow(2, scale)
        WriteSTRegister(0, result)
        SetFPUFlags(result)
    End Sub

    Private Sub ExecuteFCOMIP()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim st0 As Double = ReadSTRegister(0)
        Dim operand As Double

        If modRM._Mod = 3 Then
            ' Compare ST(0) with ST(i) and pop
            operand = ReadSTRegister(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            Select Case modRM.Reg
                Case 2 ' FCOMP single-precision
                    operand = CSng(Mem.ReadSingle(effectiveAddress))
                Case 4 ' FCOMP double-precision
                    operand = Mem.ReadQWord(effectiveAddress)
                Case Else
                    LogError("Operando FCOMP non valido")
            End Select
        End If

        If Double.IsNaN(st0) OrElse Double.IsNaN(operand) Then
            SetFPUFlags(Double.NaN)
        Else
            SetFPUFlags(st0 - operand)
        End If

        PopFPUStack() ' FCOMP always pops ST(0) after comparison
    End Sub

    Private Sub ExecuteADDPD()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim dest As Byte() = ReadXMM(modRM.Reg)
        Dim src As Byte()

        If modRM._Mod = 3 Then
            src = ReadXMM(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            src = Mem.ReadDQWord(effectiveAddress)
        End If

        Dim result(15) As Byte
        For i As Integer = 0 To 1
            Dim destValue As Double = BitConverter.ToDouble(dest, i * 8)
            Dim srcValue As Double = BitConverter.ToDouble(src, i * 8)
            Dim sum As Double = destValue + srcValue
            BitConverter.GetBytes(sum).CopyTo(result, i * 8)
        Next

        WriteXMM(modRM.Reg, result)
    End Sub

    Public Sub ExecuteADDPS()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim dest As Byte() = ReadXMM(modRM.Reg)
        Dim src As Byte()

        If modRM._Mod = 3 Then
            src = ReadXMM(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            src = Mem.ReadDQWord(effectiveAddress)
        End If

        Dim result(15) As Byte
        For i As Integer = 0 To 3
            Dim destValue As Single = BitConverter.ToSingle(dest, i * 4)
            Dim srcValue As Single = BitConverter.ToSingle(src, i * 4)
            Dim sum As Single = destValue + srcValue
            BitConverter.GetBytes(sum).CopyTo(result, i * 4)
        Next

        WriteXMM(modRM.Reg, result)
    End Sub

    Private Sub ExecuteSUBPS()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim dest As Byte() = ReadXMM(modRM.Reg)
        Dim src As Byte()

        If modRM._Mod = 3 Then
            src = ReadXMM(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            src = Mem.ReadDQWord(effectiveAddress)
        End If

        Dim result(15) As Byte
        For i As Integer = 0 To 3
            Dim destValue As Single = BitConverter.ToSingle(dest, i * 4)
            Dim srcValue As Single = BitConverter.ToSingle(src, i * 4)
            Dim difference As Single = destValue - srcValue
            BitConverter.GetBytes(difference).CopyTo(result, i * 4)
        Next

        WriteXMM(modRM.Reg, result)
    End Sub

    Private Sub ExecuteSUBPD()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim dest As Byte() = ReadXMM(modRM.Reg)
        Dim src As Byte()

        If modRM._Mod = 3 Then
            src = ReadXMM(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            src = Mem.ReadDQWord(effectiveAddress)
        End If

        Dim result(15) As Byte
        For i As Integer = 0 To 1
            Dim destValue As Double = BitConverter.ToDouble(dest, i * 8)
            Dim srcValue As Double = BitConverter.ToDouble(src, i * 8)
            Dim difference As Double = destValue - srcValue
            BitConverter.GetBytes(difference).CopyTo(result, i * 8)
        Next

        WriteXMM(modRM.Reg, result)
    End Sub

    Private Sub ExecuteMULPS()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim dest As Byte() = ReadXMM(modRM.Reg)
        Dim src As Byte()

        If modRM._Mod = 3 Then
            src = ReadXMM(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            src = Mem.ReadDQWord(effectiveAddress)
        End If

        Dim result(15) As Byte
        For i As Integer = 0 To 3
            Dim destValue As Single = BitConverter.ToSingle(dest, i * 4)
            Dim srcValue As Single = BitConverter.ToSingle(src, i * 4)
            Dim product As Single = destValue * srcValue
            BitConverter.GetBytes(product).CopyTo(result, i * 4)
        Next

        WriteXMM(modRM.Reg, result)
    End Sub

    Private Sub ExecuteMULPD()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim dest As Byte() = ReadXMM(modRM.Reg)
        Dim src As Byte()

        If modRM._Mod = 3 Then
            src = ReadXMM(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            src = Mem.ReadDQWord(effectiveAddress)
        End If

        Dim result(15) As Byte
        For i As Integer = 0 To 1
            Dim destValue As Double = BitConverter.ToDouble(dest, i * 8)
            Dim srcValue As Double = BitConverter.ToDouble(src, i * 8)
            Dim product As Double = destValue * srcValue
            BitConverter.GetBytes(product).CopyTo(result, i * 8)
        Next

        WriteXMM(modRM.Reg, result)
    End Sub

    Private Sub ExecuteDIVPS()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim dest As Byte() = ReadXMM(modRM.Reg)
        Dim src As Byte()

        If modRM._Mod = 3 Then
            src = ReadXMM(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            src = Mem.ReadDQWord(effectiveAddress)
        End If

        Dim result(15) As Byte
        For i As Integer = 0 To 3
            Dim destValue As Single = BitConverter.ToSingle(dest, i * 4)
            Dim srcValue As Single = BitConverter.ToSingle(src, i * 4)
            Dim quotient As Single = If(srcValue <> 0, destValue / srcValue, Single.NaN)
            BitConverter.GetBytes(quotient).CopyTo(result, i * 4)
        Next

        WriteXMM(modRM.Reg, result)
    End Sub

    Private Sub ExecuteDIVPD()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim dest As Byte() = ReadXMM(modRM.Reg)
        Dim src As Byte()

        If modRM._Mod = 3 Then
            src = ReadXMM(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            src = Mem.ReadDQWord(effectiveAddress)
        End If

        Dim result(15) As Byte
        For i As Integer = 0 To 1
            Dim destValue As Double = BitConverter.ToDouble(dest, i * 8)
            Dim srcValue As Double = BitConverter.ToDouble(src, i * 8)
            Dim quotient As Double = If(srcValue <> 0, destValue / srcValue, Double.NaN)
            BitConverter.GetBytes(quotient).CopyTo(result, i * 8)
        Next

        WriteXMM(modRM.Reg, result)
    End Sub

    Private Sub ExecuteMOVAPS()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim data As Byte()

        If modRM._Mod = 3 Then
            ' Registro a registro
            data = ReadXMM(modRM.RM)
            WriteXMM(modRM.Reg, data)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            ' Verifica l'allineamento
            If (effectiveAddress Mod 16) <> 0 Then
                LogError("Errore di allineamento in MOVAPS/MOVAPD")
            End If
            ' Salvataggio in memoria
            data = ReadXMM(modRM.Reg)
            Mem.WriteDQWord(effectiveAddress, data)
        End If
    End Sub

    Private Sub ExecuteMOVAPD()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim data As Byte()

        If modRM._Mod = 3 Then
            ' Registro a registro
            data = ReadXMM(modRM.RM)
            WriteXMM(modRM.Reg, data)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            ' Verifica l'allineamento
            If (effectiveAddress Mod 16) <> 0 Then
                LogError("Errore di allineamento in MOVAPS/MOVAPD")
            End If
            ' Caricamento dalla memoria
            data = Mem.ReadDQWord(effectiveAddress)
            WriteXMM(modRM.Reg, data)
        End If
    End Sub

    Private Sub ExecuteMOVUPS()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim data As Byte()

        If modRM._Mod = 3 Then
            ' Registro a registro
            data = ReadXMM(modRM.RM)
            WriteXMM(modRM.Reg, data)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            ' Salvataggio in memoria
            data = ReadXMM(modRM.Reg)
            Mem.WriteDQWord(effectiveAddress, data)
        End If
    End Sub

    Private Sub ExecuteMOVUPD()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim data As Byte()

        If modRM._Mod = 3 Then
            ' Registro a registro
            data = ReadXMM(modRM.RM)
            WriteXMM(modRM.Reg, data)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            ' Caricamento dalla memoria
            data = Mem.ReadDQWord(effectiveAddress)
            WriteXMM(modRM.Reg, data)
        End If
    End Sub

    Private Sub ExecuteANDPS()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim dest As Byte() = ReadXMM(modRM.Reg)
        Dim src As Byte()

        If modRM._Mod = 3 Then
            src = ReadXMM(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            src = Mem.ReadDQWord(effectiveAddress)
        End If

        Dim result(15) As Byte
        For i As Integer = 0 To 15
            result(i) = dest(i) And src(i)
        Next

        WriteXMM(modRM.Reg, result)
    End Sub

    Private Sub ExecuteANDPD()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim dest As Byte() = ReadXMM(modRM.Reg)
        Dim src As Byte()

        If modRM._Mod = 3 Then
            src = ReadXMM(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            src = Mem.ReadDQWord(effectiveAddress)
        End If

        Dim result(15) As Byte
        For i As Integer = 0 To 15
            result(i) = dest(i) And src(i)
        Next

        WriteXMM(modRM.Reg, result)
    End Sub

    Private Sub ExecuteORPS()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim dest As Byte() = ReadXMM(modRM.Reg)
        Dim src As Byte()

        If modRM._Mod = 3 Then
            src = ReadXMM(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            src = Mem.ReadDQWord(effectiveAddress)
        End If

        Dim result(15) As Byte
        For i As Integer = 0 To 15
            result(i) = dest(i) Or src(i)
        Next

        WriteXMM(modRM.Reg, result)
    End Sub

    Private Sub ExecuteORPD()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim dest As Byte() = ReadXMM(modRM.Reg)
        Dim src As Byte()

        If modRM._Mod = 3 Then
            src = ReadXMM(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            src = Mem.ReadDQWord(effectiveAddress)
        End If

        Dim result(15) As Byte
        For i As Integer = 0 To 15
            result(i) = dest(i) Or src(i)
        Next

        WriteXMM(modRM.Reg, result)
    End Sub

    Private Sub ExecuteXORPS()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim dest As Byte() = ReadXMM(modRM.Reg)
        Dim src As Byte()

        If modRM._Mod = 3 Then
            src = ReadXMM(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            src = Mem.ReadDQWord(effectiveAddress)
        End If

        Dim result(15) As Byte
        For i As Integer = 0 To 15
            result(i) = dest(i) Xor src(i)
        Next

        WriteXMM(modRM.Reg, result)
    End Sub

    Private Sub ExecuteXORPD()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim dest As Byte() = ReadXMM(modRM.Reg)
        Dim src As Byte()

        If modRM._Mod = 3 Then
            src = ReadXMM(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            src = Mem.ReadDQWord(effectiveAddress)
        End If

        Dim result(15) As Byte
        For i As Integer = 0 To 15
            result(i) = dest(i) Xor src(i)
        Next

        WriteXMM(modRM.Reg, result)
    End Sub

    Private Sub ExecuteCMPPS()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim dest As Byte() = ReadXMM(modRM.Reg)
        Dim src As Byte()

        If modRM._Mod = 3 Then
            src = ReadXMM(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            src = Mem.ReadDQWord(effectiveAddress)
        End If

        Dim imm8 As Byte = Mem.ReadByte(EIP)
        EIP += 1

        Dim result(15) As Byte
        For i As Integer = 0 To 3
            Dim destValue As Single = BitConverter.ToSingle(dest, i * 4)
            Dim srcValue As Single = BitConverter.ToSingle(src, i * 4)
            Dim compResult As Boolean
            Select Case imm8
                Case 0 : compResult = destValue = srcValue
                Case 1 : compResult = destValue < srcValue
                Case 2 : compResult = destValue <= srcValue
                Case 3 : compResult = Single.IsNaN(destValue) OrElse Single.IsNaN(srcValue)
                Case 4 : compResult = destValue <> srcValue
                Case 5 : compResult = destValue >= srcValue
                Case 6 : compResult = destValue > srcValue
                Case 7 : compResult = Not (Single.IsNaN(destValue) OrElse Single.IsNaN(srcValue))
                Case Else : LogError("Invalid CMPPS comparison type")
            End Select
            Dim compValue As UInteger = If(compResult, UInteger.MaxValue, 0)
            BitConverter.GetBytes(compValue).CopyTo(result, i * 4)
        Next

        WriteXMM(modRM.Reg, result)
    End Sub

    Private Sub ExecuteCMPPD()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim dest As Byte() = ReadXMM(modRM.Reg)
        Dim src As Byte()

        If modRM._Mod = 3 Then
            src = ReadXMM(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            src = Mem.ReadDQWord(effectiveAddress)
        End If

        Dim imm8 As Byte = Mem.ReadByte(EIP)
        EIP += 1

        Dim result(15) As Byte
        For i As Integer = 0 To 1
            Dim destValue As Double = BitConverter.ToDouble(dest, i * 8)
            Dim srcValue As Double = BitConverter.ToDouble(src, i * 8)
            Dim compResult As Boolean
            Select Case imm8
                Case 0 : compResult = destValue = srcValue
                Case 1 : compResult = destValue < srcValue
                Case 2 : compResult = destValue <= srcValue
                Case 3 : compResult = Double.IsNaN(destValue) OrElse Double.IsNaN(srcValue)
                Case 4 : compResult = destValue <> srcValue
                Case 5 : compResult = destValue >= srcValue
                Case 6 : compResult = destValue > srcValue
                Case 7 : compResult = Not (Double.IsNaN(destValue) OrElse Double.IsNaN(srcValue))
                Case Else : LogError("Invalid CMPPD comparison type")
            End Select
            Dim compValue As ULong = If(compResult, ULong.MaxValue, 0)
            BitConverter.GetBytes(compValue).CopyTo(result, i * 8)
        Next

        WriteXMM(modRM.Reg, result)
    End Sub

    Private Sub ExecuteMINPS()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim dest As Byte() = ReadXMM(modRM.Reg)
        Dim src As Byte()

        If modRM._Mod = 3 Then
            src = ReadXMM(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            src = Mem.ReadDQWord(effectiveAddress)
        End If

        Dim result(15) As Byte
        For i As Integer = 0 To 3
            Dim destValue As Single = BitConverter.ToSingle(dest, i * 4)
            Dim srcValue As Single = BitConverter.ToSingle(src, i * 4)
            Dim minValue As Single = Math.Min(destValue, srcValue)
            BitConverter.GetBytes(minValue).CopyTo(result, i * 4)
        Next

        WriteXMM(modRM.Reg, result)
    End Sub

    Private Sub ExecuteMINPD()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim dest As Byte() = ReadXMM(modRM.Reg)
        Dim src As Byte()

        If modRM._Mod = 3 Then
            src = ReadXMM(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            src = Mem.ReadDQWord(effectiveAddress)
        End If

        Dim result(15) As Byte
        For i As Integer = 0 To 1
            Dim destValue As Double = BitConverter.ToDouble(dest, i * 8)
            Dim srcValue As Double = BitConverter.ToDouble(src, i * 8)
            Dim minValue As Double = Math.Min(destValue, srcValue)
            BitConverter.GetBytes(minValue).CopyTo(result, i * 8)
        Next

        WriteXMM(modRM.Reg, result)
    End Sub

    Private Sub ExecuteMAXPS()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim dest As Byte() = ReadXMM(modRM.Reg)
        Dim src As Byte()

        If modRM._Mod = 3 Then
            src = ReadXMM(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            src = Mem.ReadDQWord(effectiveAddress)
        End If

        Dim result(15) As Byte
        For i As Integer = 0 To 3
            Dim destValue As Single = BitConverter.ToSingle(dest, i * 4)
            Dim srcValue As Single = BitConverter.ToSingle(src, i * 4)
            Dim maxValue As Single = Math.Max(destValue, srcValue)
            BitConverter.GetBytes(maxValue).CopyTo(result, i * 4)
        Next

        WriteXMM(modRM.Reg, result)
    End Sub

    Private Sub ExecuteMAXPD()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim dest As Byte() = ReadXMM(modRM.Reg)
        Dim src As Byte()

        If modRM._Mod = 3 Then
            src = ReadXMM(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            src = Mem.ReadDQWord(effectiveAddress)
        End If

        Dim result(15) As Byte
        For i As Integer = 0 To 1
            Dim destValue As Double = BitConverter.ToDouble(dest, i * 8)
            Dim srcValue As Double = BitConverter.ToDouble(src, i * 8)
            Dim maxValue As Double = Math.Max(destValue, srcValue)
            BitConverter.GetBytes(maxValue).CopyTo(result, i * 8)
        Next

        WriteXMM(modRM.Reg, result)
    End Sub

    Private Sub ExecuteSHUFPS()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim dest As Byte() = ReadXMM(modRM.Reg)
        Dim src As Byte()

        If modRM._Mod = 3 Then
            src = ReadXMM(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            src = Mem.ReadDQWord(effectiveAddress)
        End If

        Dim imm8 As Byte = Mem.ReadByte(EIP)
        EIP += 1

        Dim result(15) As Byte
        For i As Integer = 0 To 3
            Dim selectIndex As Integer = (imm8 >> (i * 2)) And 3
            Dim sourceArray As Byte() = If(i < 2, dest, src)
            Dim sourceIndex As Integer = If(i < 2, selectIndex, selectIndex And 1)
            Array.Copy(sourceArray, sourceIndex * 4, result, i * 4, 4)
        Next

        WriteXMM(modRM.Reg, result)
    End Sub

    Private Sub ExecuteSHUFPD()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim dest As Byte() = ReadXMM(modRM.Reg)
        Dim src As Byte()

        If modRM._Mod = 3 Then
            src = ReadXMM(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            src = Mem.ReadDQWord(effectiveAddress)
        End If

        Dim imm8 As Byte = Mem.ReadByte(EIP)
        EIP += 1

        Dim result(15) As Byte
        Array.Copy(If((imm8 And 1) = 0, dest, src), 0, result, 0, 8)
        Array.Copy(If((imm8 And 2) = 0, src, dest), 8, result, 8, 8)

        WriteXMM(modRM.Reg, result)
    End Sub

    Private Sub ExecuteUNPCKLPS()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim dest As Byte() = ReadXMM(modRM.Reg)
        Dim src As Byte()

        If modRM._Mod = 3 Then
            src = ReadXMM(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            src = Mem.ReadDQWord(effectiveAddress)
        End If

        Dim result(15) As Byte
        Array.Copy(dest, 0, result, 0, 4)
        Array.Copy(src, 0, result, 4, 4)
        Array.Copy(dest, 4, result, 8, 4)
        Array.Copy(src, 4, result, 12, 4)

        WriteXMM(modRM.Reg, result)
    End Sub

    Private Sub ExecuteUNPCKLPD()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim dest As Byte() = ReadXMM(modRM.Reg)
        Dim src As Byte()

        If modRM._Mod = 3 Then
            src = ReadXMM(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            src = Mem.ReadDQWord(effectiveAddress)
        End If

        Dim result(15) As Byte
        Array.Copy(dest, 0, result, 0, 8)
        Array.Copy(src, 0, result, 8, 8)

        WriteXMM(modRM.Reg, result)
    End Sub

    Private Sub ExecuteUNPCKHPS()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim dest As Byte() = ReadXMM(modRM.Reg)
        Dim src As Byte()

        If modRM._Mod = 3 Then
            src = ReadXMM(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            src = Mem.ReadDQWord(effectiveAddress)
        End If

        Dim result(15) As Byte
        Array.Copy(dest, 8, result, 0, 4)
        Array.Copy(src, 8, result, 4, 4)
        Array.Copy(dest, 12, result, 8, 4)
        Array.Copy(src, 12, result, 12, 4)

        WriteXMM(modRM.Reg, result)
    End Sub

    Private Sub ExecuteUNPCKHPD()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim dest As Byte() = ReadXMM(modRM.Reg)
        Dim src As Byte()

        If modRM._Mod = 3 Then
            src = ReadXMM(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            src = Mem.ReadDQWord(effectiveAddress)
        End If

        Dim result(15) As Byte
        Array.Copy(dest, 8, result, 0, 8)
        Array.Copy(src, 8, result, 8, 8)

        WriteXMM(modRM.Reg, result)
    End Sub

    Private Sub ExecuteCVTPS2PD()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim src As Byte()

        If modRM._Mod = 3 Then
            src = ReadXMM(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            src = Mem.ReadDQWord(effectiveAddress)
        End If

        Dim result(15) As Byte
        For i As Integer = 0 To 1
            Dim singleValue As Single = BitConverter.ToSingle(src, i * 4)
            Dim doubleValue As Double = CDbl(singleValue)
            BitConverter.GetBytes(doubleValue).CopyTo(result, i * 8)
        Next

        WriteXMM(modRM.Reg, result)
    End Sub

    Private Sub ExecuteCVTPD2PS()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim src As Byte()

        If modRM._Mod = 3 Then
            src = ReadXMM(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            src = Mem.ReadDQWord(effectiveAddress)
        End If

        Dim result(15) As Byte
        For i As Integer = 0 To 1
            Dim doubleValue As Double = BitConverter.ToDouble(src, i * 8)
            Dim singleValue As Single = CSng(doubleValue)
            BitConverter.GetBytes(singleValue).CopyTo(result, i * 4)
        Next
        Array.Clear(result, 8, 8) ' Clear upper 64 bits

        WriteXMM(modRM.Reg, result)
    End Sub

    Private Sub ExecuteCVTSI2SS()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim dest As Byte() = ReadXMM(modRM.Reg)
        Dim intValue As Integer

        If modRM._Mod = 3 Then
            intValue = CInt(ReadRegister32(modRM.RM))
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            intValue = CInt(Mem.ReadDWord(effectiveAddress))
        End If

        Dim singleValue As Single = CSng(intValue)
        BitConverter.GetBytes(singleValue).CopyTo(dest, 0)

        WriteXMM(modRM.Reg, dest)
    End Sub

    Private Sub ExecuteCVTSI2SD()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim dest As Byte() = ReadXMM(modRM.Reg)
        Dim intValue As Integer

        If modRM._Mod = 3 Then
            intValue = CInt(ReadRegister32(modRM.RM))
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            intValue = CInt(Mem.ReadDWord(effectiveAddress))
        End If

        Dim doubleValue As Double = CDbl(intValue)
        BitConverter.GetBytes(doubleValue).CopyTo(dest, 0)

        WriteXMM(modRM.Reg, dest)
    End Sub

    Private Sub ExecuteEXTRACTPS()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim src As Byte() = ReadXMM(modRM.Reg)

        Dim imm8 As Byte = Mem.ReadByte(EIP)
        EIP += 1

        Dim index As Integer = imm8 And 3
        Dim extractedValue As Single = BitConverter.ToSingle(src, index * 4)

        If modRM._Mod = 3 Then
            WriteRegister32(modRM.RM, BitConverter.ToUInt32(BitConverter.GetBytes(extractedValue), 0))
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            Mem.WriteDWord(effectiveAddress, BitConverter.ToUInt32(BitConverter.GetBytes(extractedValue), 0))
        End If
    End Sub

    Private Sub ExecuteINSERTPS()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim dest As Byte() = ReadXMM(modRM.Reg)
        Dim src As Byte()

        If modRM._Mod = 3 Then
            src = ReadXMM(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            src = New Byte(15) {}
            BitConverter.GetBytes(Mem.ReadDWord(effectiveAddress)).CopyTo(src, 0)
        End If

        Dim imm8 As Byte = Mem.ReadByte(EIP)
        EIP += 1

        Dim srcIndex As Integer = (imm8 >> 6) And 3
        Dim destIndex As Integer = (imm8 >> 4) And 3
        Dim zeroMask As Byte = CByte(imm8 And &HF)

        ' Insert the selected single-precision float
        Array.Copy(src, srcIndex * 4, dest, destIndex * 4, 4)

        ' Apply zero mask
        For i As Integer = 0 To 3
            If (zeroMask And (1 << i)) <> 0 Then
                Array.Clear(dest, i * 4, 4)
            End If
        Next

        WriteXMM(modRM.Reg, dest)
    End Sub

    Private Sub ExecutePADDD()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim dest As Byte() = ReadXMM(modRM.Reg)
        Dim src As Byte()

        If modRM._Mod = 3 Then
            src = ReadXMM(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            src = Mem.ReadDQWord(effectiveAddress)
        End If

        Dim result(15) As Byte
        For i As Integer = 0 To 3
            Dim destValue As UInteger = BitConverter.ToUInt32(dest, i * 4)
            Dim srcValue As UInteger = BitConverter.ToUInt32(src, i * 4)
            Dim sum As UInteger = destValue + srcValue
            BitConverter.GetBytes(sum).CopyTo(result, i * 4)
        Next

        WriteXMM(modRM.Reg, result)
    End Sub

    Private Sub ExecutePSUBD()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim dest As Byte() = ReadXMM(modRM.Reg)
        Dim src As Byte()

        If modRM._Mod = 3 Then
            src = ReadXMM(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            src = Mem.ReadDQWord(effectiveAddress)
        End If

        Dim result(15) As Byte
        For i As Integer = 0 To 3
            Dim destValue As UInteger = BitConverter.ToUInt32(dest, i * 4)
            Dim srcValue As UInteger = BitConverter.ToUInt32(src, i * 4)
            Dim difference As UInteger = destValue - srcValue
            BitConverter.GetBytes(difference).CopyTo(result, i * 4)
        Next

        WriteXMM(modRM.Reg, result)
    End Sub

    Private Sub ExecutePMULLD()
        Dim modRM As ModRMByte = DecodeModRM()
        Dim dest As Byte() = ReadXMM(modRM.Reg)
        Dim src As Byte()

        If modRM._Mod = 3 Then
            src = ReadXMM(modRM.RM)
        Else
            Dim effectiveAddress As UInteger = DecodeEffectiveAddress(modRM)
            src = Mem.ReadDQWord(effectiveAddress)
        End If

        Dim result(15) As Byte
        For i As Integer = 0 To 3
            Dim destValue As Integer = BitConverter.ToInt32(dest, i * 4)
            Dim srcValue As Integer = BitConverter.ToInt32(src, i * 4)
            Dim product As Integer = destValue * srcValue
            BitConverter.GetBytes(product).CopyTo(result, i * 4)
        Next

        WriteXMM(modRM.Reg, result)
    End Sub

End Class

