Module AsmOpcode

    ' Definizione di costanti per i prefissi degli opcode
    Public Const PREFIX_LOCK As UInteger = &HF0
    Public Const PREFIX_REPNE As UInteger = &HF2
    Public Const PREFIX_REPNZ As UInteger = &HF2
    Public Const PREFIX_REPE As UInteger = &HF3
    Public Const PREFIX_REPZ As UInteger = &HF3
    Public Const PREFIX_REP As UInteger = &HF3
    Public Const PREFIX_OPERAND_SIZE_OVERRIDE As UInteger = &H66
    Public Const PREFIX_ADDRESS_SIZE_OVERRIDE As UInteger = &H67

    ' Definizione di costanti per le condizioni
    Public Const CONDITIONS As String =
"A  B  C  E  G  L  S  Z  O  P  " &
"AE BE GE LE NA NB NC NE NG NL " &
"NO NP NS NZ PE PO NAE NBE NGE " &
"NLE"

    ' Definizione di costanti per i registri generali
    Public Const Registers As String =
"AL  BL  CL  DL  AH  BH  CH  DH " &
"AX  BX  CX  DX  BP  SP  DI  SI " &
"EAX EBX ECX EDX EBP ESP EDI ESI"

    ' Definizione di costanti per i registri della FPU
    Public Const FPU_REGS As String =
"ST(0) ST(1) ST(2) ST(3) ST(4) ST(5) ST(6) ST(7)"

    ' Definizione di costanti per i registri MM e XMM
    Public Const MM_REGS As String =
"MM0 MM1 MM2 MM3 MM4 MM5 MM6 MM7 " &
"XMM0 XMM1 XMM2 XMM3 XMM4 XMM5 XMM6 XMM7"

    ' Definizione di costanti per le parole chiave
    Public Const KEYWORDS As String =
"BYTE WORD DWORD QWORD DQWORD FLOAT " &
"DOUBLE EXTENDED " &
"LOCK REPNE REPNZ REPE REP"

    ' Definizione di costanti per i dati grezzi
    Public Const RAW_DATA As String =
"DB DW DD"

    ' Definizione di un enum per i prefissi degli opcode
    Public Enum OpCodePrefixes
        PrefixNone = &H0&
        PrefixFlgLock = &H1&
        PrefixFlgRepne = &H2&
        PrefixFlgRepnz = &H2&
        PrefixFlgRep = &H4&
        PrefixFlgRepe = &H4&
        PrefixFlgRepz = &H4&
        PrefixFlgOperandSizeOverride = &H800&
        PrefixFlgAddressSizeOverride = &H1000&
    End Enum

    ' Definizione di un enum per i tipi di parametri
    Public Enum ParamType
        ParamTypeUnknown = &H0
        ParamReg = &H1
        ParamRel = &H2
        ParamMem = &H4
        ParamImm = &H8
        ParamSTX = &H10
        ParamMM = &H20
        ParamExt = &H40
    End Enum

    ' Definizione di un enum per le dimensioni dei parametri
    Public Enum ParamSize
        BitsUnknown = 0
        Bits8 = 8
        Bits16 = 16
        Bits32 = 32
        Bits64 = 64
        Bits80 = 80
        Bits128 = 128
    End Enum

    ' Definizione di un enum per i tipi di estensione
    Private Enum ExtType
        ExtNon = 0
        ExtFlt
        ExtReg
        ExtCon
    End Enum

    ' Definizione di un enum per la modifica delle dimensioni
    Private Enum SizeMod
        SizeModOvrd
        SizeModNone
    End Enum

    ' Definizione di un enum per i registri ASM
    Public Enum ASMRegisters
        RegUnknown = &H0&
        RegAL = &H1&
        RegBL = &H2&
        RegCL = &H4&
        RegDL = &H8&
        RegAH = &H10&
        RegBH = &H20&
        RegCH = &H40&
        RegDH = &H80&
        RegAX = &H100&
        RegBX = &H200&
        RegCX = &H400&
        RegDX = &H800&
        RegBP = &H1000&
        RegSP = &H2000&
        RegDI = &H4000&
        RegSI = &H8000&
        RegEAX = &H10000
        RegEBX = &H20000
        RegECX = &H40000
        RegEDX = &H80000
        RegEBP = &H100000
        RegESP = &H200000
        RegEDI = &H400000
        RegESI = &H800000
    End Enum

    ' Definizione di un enum per i registri FPU ASM
    Public Enum ASMFPURegisters
        FP_UNKNOWN = -1
        FP_ST0 = 0
        FP_ST1
        FP_ST2
        FP_ST3uad
        FP_ST4
        FP_ST5
        FP_ST6
        FP_ST7
    End Enum

    ' Definizione di un enum per i registri MM e XMM ASM
    Public Enum ASMXMMRegisters
        MM_Unknown = -1
        MM0 = &H1&
        MM1 = &H2&
        MM2 = &H4&
        MM3 = &H8&
        MM4 = &H10&
        MM5 = &H20&
        MM6 = &H40&
        MM7 = &H80&
        XMM0 = &H100&
        XMM1 = &H200&
        XMM2 = &H400&
        XMM3 = &H800&
        XMM4 = &H1000&
        XMM5 = &H2000&
        XMM6 = &H4000&
        XMM7 = &H8000&
    End Enum

    Public Class ParsedInstruction
        Public Property Label As String
        Public Property Mnemonic As String
        Public Property Operands As List(Of ParsedOperand)
        Public Property Comment As String
        Public Property Prefixes As List(Of Byte)
    End Class

    Public Class ParsedOperand
        Public Property Type As ParamType
        Public Property Value As Object
        Public Property Size As ParamSize
        Public Property Qualifier As String
    End Class

    Public Class OffsetOperand
        Public Property Symbol As String

        Public Sub New(symbol As String)
            Me.Symbol = symbol
        End Sub

        Public Overrides Function ToString() As String
            Return $"OFFSET {Symbol}"
        End Function
    End Class

    Public Class MemoryOperand
        Public Property Size As String
        Public Property BaseRegister As String
        Public Property IndexRegister As String
        Public Property Scale As Integer
        Public Property Displacement As Integer
        Public Property Label As String

        Public Overrides Function ToString() As String
            Dim parts As New List(Of String)

            If Not String.IsNullOrEmpty(Label) Then
                parts.Add(Label)
            End If

            If Not String.IsNullOrEmpty(BaseRegister) Then
                If parts.Count > 0 Then parts.Add("+")
                parts.Add(BaseRegister)
            End If

            If Not String.IsNullOrEmpty(IndexRegister) Then
                If parts.Count > 0 Then parts.Add("+")
                parts.Add(IndexRegister)
                If Scale > 1 Then
                    parts.Add($"*{Scale}")
                End If
            End If

            If Displacement <> 0 Then
                If Displacement > 0 AndAlso parts.Count > 0 Then
                    parts.Add("+")
                End If
                parts.Add(Displacement.ToString())
            End If

            Return String.Join(" ", parts)
        End Function
    End Class

    ' Variabili per memorizzare i vari componenti delle istruzioni
    Private m_strConditions() As String
    Private m_strRegisters() As String
    Private m_strFPURegs() As String
    Private m_strMMRegs() As String


    Public Function SizesForInt(ByVal lngVal As UInteger) As ParamSize
        If (lngVal >= -128 And lngVal <= 255) Then
            Return ParamSize.Bits8 Or ParamSize.Bits16 Or ParamSize.Bits32 Or ParamSize.Bits64 Or ParamSize.Bits80
        ElseIf (lngVal >= -32768 And lngVal <= 65535) Then
            Return ParamSize.Bits16 Or ParamSize.Bits32 Or ParamSize.Bits64 Or ParamSize.Bits80
        Else
            Return ParamSize.Bits32 Or ParamSize.Bits64 Or ParamSize.Bits80
        End If
    End Function

    Public Function IsFPUReg(strReg As String) As Boolean
        Dim i As UInteger
        m_strFPURegs = Split(FPU_REGS, " ")
        For i = 0 To UBound(m_strFPURegs)
            If StrComp(m_strFPURegs(i), strReg, vbTextCompare) = 0 Then
                Return True
            End If
        Next
        Return False
    End Function

    Public Function IsRegister(strReg As String) As Boolean
        Dim i As UInteger
        m_strRegisters = Split(Registers, " ")

        For i = 0 To UBound(m_strRegisters)
            If StrComp(m_strRegisters(i), strReg, vbTextCompare) = 0 Then
                Return True
            End If
        Next
        Return False
    End Function

    Public Function IsMMReg(strReg As String) As Boolean
        Dim i As UInteger
        m_strMMRegs = Split(MM_REGS, " ")

        For i = 0 To UBound(m_strMMRegs)
            If StrComp(m_strMMRegs(i), strReg, vbTextCompare) = 0 Then
                Return True
            End If
        Next
        Return False
    End Function

    Public Function FPURegStrToNum(strST As String) As ASMFPURegisters
        If UCase(Left(strST, 3)) = "ST(" Then
            Return CLng(Mid(strST, 4, 1))
        Else
            Return ASMFPURegisters.FP_UNKNOWN
        End If
    End Function


    Public Function MMRegStrToNum(strMM As String) As ASMXMMRegisters
        Dim udeBase As New ASMXMMRegisters
        Dim strNum As String
        If UCase(Left(strMM, 3)) = "XMM" Then
            udeBase = ASMXMMRegisters.XMM0
            strNum = Mid(strMM, 4, 1)
        ElseIf UCase(Left(strMM, 2)) = "MM" Then
            udeBase = ASMXMMRegisters.MM0
            strNum = Mid(strMM, 3, 1)
        Else
            Console.WriteLine("Kein (X)MM Register")
        End If

        If IsNumeric(strNum) Then
            udeBase = udeBase * 2 ^ CInt(strNum)
        Else
            If udeBase = ASMXMMRegisters.XMM0 Then
                udeBase = ASMXMMRegisters.XMM0 Or ASMXMMRegisters.XMM1 Or ASMXMMRegisters.XMM2 Or ASMXMMRegisters.XMM3 Or ASMXMMRegisters.XMM4 Or ASMXMMRegisters.XMM5 Or ASMXMMRegisters.XMM6 Or ASMXMMRegisters.XMM7
            Else
                udeBase = ASMXMMRegisters.MM0 Or ASMXMMRegisters.MM1 Or ASMXMMRegisters.MM2 Or ASMXMMRegisters.MM3 Or ASMXMMRegisters.MM4 Or ASMXMMRegisters.MM5 Or ASMXMMRegisters.MM6 Or ASMXMMRegisters.MM7
            End If
        End If

        Return udeBase
    End Function


    ' offset to add to the opcode of a conditional Instruction(for a specific condition
    Public Function ConditionOffset(cc As String) As UInteger
        Select Case UCase(cc)
            Case "O" : Return 0
            Case "NO" : Return 1
            Case "B", "C", "NAE" : Return 2
            Case "AE", "NB", "NC" : Return 3
            Case "E", "Z" : Return 4
            Case "NE", "NZ" : Return 5
            Case "BE", "NA" : Return 6
            Case "A", "NBE" : Return 7
            Case "S" : Return 8
            Case "NS" : Return 9
            Case "P", "PE" : Return 10
            Case "NP", "PO" : Return 11
            Case "L", "NGE" : Return 12
            Case "GE", "NL" : Return 13
            Case "LE", "NG" : Return 14
            Case "G", "NLE" : Return 15
        End Select
    End Function


    Public Function GetRegExtRegName(ByVal Offset As UInteger, ByVal size As ParamSize) As String
        Select Case size
            Case ParamSize.Bits8, ParamSize.Bits16, ParamSize.Bits32
            Case Else : Console.WriteLine("GetRegExtRegName: invalid size")
        End Select

        Select Case Offset
            Case 0
                Select Case size
                    Case ParamSize.Bits8 : Return "AL"
                    Case ParamSize.Bits16 : Return "AX"
                    Case ParamSize.Bits32 : Return "EAX"
                End Select
            Case 1
                Select Case size
                    Case ParamSize.Bits8 : Return "CL"
                    Case ParamSize.Bits16 : Return "CX"
                    Case ParamSize.Bits32 : Return "ECX"
                End Select
            Case 2
                Select Case size
                    Case ParamSize.Bits8 : Return "DL"
                    Case ParamSize.Bits16 : Return "DX"
                    Case ParamSize.Bits32 : Return "EDX"
                End Select
            Case 3
                Select Case size
                    Case ParamSize.Bits8 : Return "BL"
                    Case ParamSize.Bits16 : Return "BX"
                    Case ParamSize.Bits32 : Return "EBX"
                End Select
            Case 4
                Select Case size
                    Case ParamSize.Bits8 : Return "AH"
                    Case ParamSize.Bits16 : Return "SP"
                    Case ParamSize.Bits32 : Return "ESP"
                End Select
            Case 5
                Select Case size
                    Case ParamSize.Bits8 : Return "CH"
                    Case ParamSize.Bits16 : Return "BP"
                    Case ParamSize.Bits32 : Return "EBP"
                End Select
            Case 6
                Select Case size
                    Case ParamSize.Bits8 : Return "DH"
                    Case ParamSize.Bits16 : Return "SI"
                    Case ParamSize.Bits32 : Return "ESI"
                End Select
            Case 7
                Select Case size
                    Case ParamSize.Bits8 : Return "BH"
                    Case ParamSize.Bits16 : Return "DI"
                    Case ParamSize.Bits32 : Return "EDI"
                End Select
            Case Else
                Console.WriteLine("GetRegExtRegName: Invalid offset")
        End Select
    End Function


    Public Function RegisterSize(reg As ASMRegisters) As ParamSize
        Select Case reg
            Case ASMRegisters.RegAL, ASMRegisters.RegAH, ASMRegisters.RegBL, ASMRegisters.RegBH, ASMRegisters.RegCL, ASMRegisters.RegCH, ASMRegisters.RegDL, ASMRegisters.RegDH
                Return ParamSize.Bits8
            Case ASMRegisters.RegAX, ASMRegisters.RegBX, ASMRegisters.RegCX, ASMRegisters.RegDX, ASMRegisters.RegBP, ASMRegisters.RegSP, ASMRegisters.RegDI, ASMRegisters.RegSI
                Return ParamSize.Bits16
            Case ASMRegisters.RegEAX, ASMRegisters.RegEBX, ASMRegisters.RegECX, ASMRegisters.RegEDX, ASMRegisters.RegEBP, ASMRegisters.RegESP, ASMRegisters.RegEDI, ASMRegisters.RegESI
                Return ParamSize.Bits32
            Case Else
                Return ParamSize.BitsUnknown
        End Select
    End Function

    Public Function RegStrToReg(strReg As String) As ASMRegisters
        Select Case LCase(strReg)
            Case "al" : Return ASMRegisters.RegAL
            Case "ah" : Return ASMRegisters.RegAH
            Case "bl" : Return ASMRegisters.RegBL
            Case "bh" : Return ASMRegisters.RegBH
            Case "cl" : Return ASMRegisters.RegCL
            Case "ch" : Return ASMRegisters.RegCH
            Case "dl" : Return ASMRegisters.RegDL
            Case "dh" : Return ASMRegisters.RegDH
            Case "sp" : Return ASMRegisters.RegSP
            Case "bp" : Return ASMRegisters.RegBP
            Case "si" : Return ASMRegisters.RegSI
            Case "di" : Return ASMRegisters.RegDI
            Case "ax" : Return ASMRegisters.RegAX
            Case "bx" : Return ASMRegisters.RegBX
            Case "cx" : Return ASMRegisters.RegCX
            Case "dx" : Return ASMRegisters.RegDX
            Case "eax" : Return ASMRegisters.RegEAX
            Case "ebx" : Return ASMRegisters.RegEBX
            Case "ecx" : Return ASMRegisters.RegECX
            Case "edx" : Return ASMRegisters.RegEDX
            Case "esp" : Return ASMRegisters.RegESP
            Case "ebp" : Return ASMRegisters.RegEBP
            Case "esi" : Return ASMRegisters.RegESI
            Case "edi" : Return ASMRegisters.RegEDI
            Case Else : Return ASMRegisters.RegUnknown
        End Select
    End Function

    Public Function RegToBin(ByVal Strs As ASMRegisters) As Byte
        Dim strx As String = ""
        Select Case Strs
            Case ASMRegisters.RegAL : strx = 0
            Case ASMRegisters.RegCL : strx = 1
            Case ASMRegisters.RegDL : strx = 2
            Case ASMRegisters.RegBL : strx = 3
            Case ASMRegisters.RegAH : strx = 4
            Case ASMRegisters.RegCH : strx = 5
            Case ASMRegisters.RegDH : strx = 6
            Case ASMRegisters.RegBH : strx = 7
            Case ASMRegisters.RegAX : strx = 0
            Case ASMRegisters.RegCX : strx = 1
            Case ASMRegisters.RegDX : strx = 2
            Case ASMRegisters.RegBX : strx = 3
            Case ASMRegisters.RegSP : strx = 4
            Case ASMRegisters.RegBP : strx = 5
            Case ASMRegisters.RegSI : strx = 6
            Case ASMRegisters.RegDI : strx = 7
            Case ASMRegisters.RegEAX : strx = 0
            Case ASMRegisters.RegECX : strx = 1
            Case ASMRegisters.RegEDX : strx = 2
            Case ASMRegisters.RegEBX : strx = 3
            Case ASMRegisters.RegESP : strx = 4
            Case ASMRegisters.RegEBP : strx = 5
            Case ASMRegisters.RegESI : strx = 6
            Case ASMRegisters.RegEDI : strx = 7
        End Select
        Return strx
    End Function

    Public Function MMRegStrToReg(strReg As String) As ASMXMMRegisters
        Select Case LCase(strReg)
            Case "mm0" : Return ASMXMMRegisters.MM0
            Case "mm1" : Return ASMXMMRegisters.MM1
            Case "mm2" : Return ASMXMMRegisters.MM2
            Case "mm3" : Return ASMXMMRegisters.MM3
            Case "mm4" : Return ASMXMMRegisters.MM4
            Case "mm5" : Return ASMXMMRegisters.MM5
            Case "mm6" : Return ASMXMMRegisters.MM6
            Case "mm7" : Return ASMXMMRegisters.MM7
            Case "xmm0" : Return ASMXMMRegisters.XMM0
            Case "xmm1" : Return ASMXMMRegisters.XMM1
            Case "xmm2" : Return ASMXMMRegisters.XMM2
            Case "xmm3" : Return ASMXMMRegisters.XMM3
            Case "xmm4" : Return ASMXMMRegisters.XMM4
            Case "xmm5" : Return ASMXMMRegisters.XMM5
            Case "xmm6" : Return ASMXMMRegisters.XMM6
            Case "xmm7" : Return ASMXMMRegisters.XMM7
            Case Else : Return ASMXMMRegisters.MM_Unknown
        End Select
    End Function

    Public Function MMRegToBin(ByVal Reg As ASMXMMRegisters) As Byte
        Select Case Reg
            Case ASMXMMRegisters.MM0, ASMXMMRegisters.XMM0 : Return 0
            Case ASMXMMRegisters.MM1, ASMXMMRegisters.XMM1 : Return 1
            Case ASMXMMRegisters.MM2, ASMXMMRegisters.XMM2 : Return 2
            Case ASMXMMRegisters.MM3, ASMXMMRegisters.XMM3 : Return 3
            Case ASMXMMRegisters.MM4, ASMXMMRegisters.XMM4 : Return 4
            Case ASMXMMRegisters.MM5, ASMXMMRegisters.XMM5 : Return 5
            Case ASMXMMRegisters.MM6, ASMXMMRegisters.XMM6 : Return 6
            Case ASMXMMRegisters.MM7, ASMXMMRegisters.XMM7 : Return 7
            Case Else : Return 255 ' Valore per indicare un registro sconosciuto
        End Select
    End Function

    Public Function STXRegStrToReg(strReg As String) As ASMFPURegisters
        Select Case LCase(strReg)
            Case "ST(0)" : Return ASMFPURegisters.FP_ST0
            Case "ST(1)" : Return ASMFPURegisters.FP_ST1
            Case "ST(2)" : Return ASMFPURegisters.FP_ST2
            Case "ST(3)" : Return ASMFPURegisters.FP_ST3uad
            Case "ST(4)" : Return ASMFPURegisters.FP_ST4
            Case "ST(5)" : Return ASMFPURegisters.FP_ST5
            Case "ST(6)" : Return ASMFPURegisters.FP_ST6
            Case "ST(7)" : Return ASMFPURegisters.FP_ST7
            Case Else : Return ASMFPURegisters.FP_UNKNOWN
        End Select
    End Function

    Public Function STXRegToBin(ByVal Reg As ASMFPURegisters) As Byte
        Select Case Reg
            Case ASMFPURegisters.FP_ST0 : Return 0
            Case ASMFPURegisters.FP_ST1 : Return 1
            Case ASMFPURegisters.FP_ST2 : Return 2
            Case ASMFPURegisters.FP_ST3uad : Return 3
            Case ASMFPURegisters.FP_ST4 : Return 4
            Case ASMFPURegisters.FP_ST5 : Return 5
            Case ASMFPURegisters.FP_ST6 : Return 6
            Case ASMFPURegisters.FP_ST7 : Return 7
            Case Else : Return 255 ' Valore per indicare un registro sconosciuto
        End Select
    End Function


    Public Function ImmediateType(value As Long) As ParamSize
        If value >= SByte.MinValue AndAlso value <= Byte.MaxValue Then
            Return ParamSize.Bits8
        ElseIf value >= UShort.MinValue AndAlso value <= UShort.MaxValue Then
            Return ParamSize.Bits32
        ElseIf value >= Short.MinValue AndAlso value <= Short.MaxValue Then
            Return ParamSize.Bits32
        ElseIf value >= UInteger.MinValue AndAlso value <= UInteger.MaxValue Then
            Return ParamSize.Bits32
        ElseIf value >= Integer.MinValue AndAlso value <= Integer.MaxValue Then
            Return ParamSize.Bits32
        Else
            Return ParamSize.BitsUnknown ' Long
        End If
    End Function



End Module
