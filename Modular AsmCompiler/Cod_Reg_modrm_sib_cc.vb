Module Cod_Reg_modrm_sib_cc
    'mod_table
    '00 -> memory mode senza immediato es. [bx+si] - tranne nel caso in cui r/m = 110 per cui immediato a 16 bit
    '01 -> memory mode con immediato a 8 bit es. [bx+imm8]
    '10 -> memory mode con immediato a 16 bit es. [imm16]
    '11 -> operazioni tra registri es ax, bx

    '     reg_table      W=0 [66h] W=1    w=0  w=1   66h  non_66h
    ' al_ax_eax = "000"  'al   |  ax      al   eax   ax    eax
    ' cl_cx_ecx = "001"  'cl   |  cx      cl   ecx   cx    ecx
    ' dl_dx_edx = "010"  'dl   |  dx      dl   edx   dx    edx
    ' bl_bx_ebx = "011"  'bl   |  bx      bl   ebx   bx    ebx
    ' ah_sp_esp = "100"  'ah   |  sp      ah   esp   sp    esp
    ' ch_bp_ebp = "101"  'ch   |  bp      ch   ebp   bp    ebp
    ' dh_si_esi = "110"  'dh   |  si      dh   esi   si    esi
    ' bh_di_edi = "111"  'bh   |  di      bh   edi   di    edi

    Public Function Reg8ToBin(ByVal Strs As String) As Byte
        Dim strx As String = ""
        Select Case Strs
            Case "AL" : strx = 0
            Case "CL" : strx = 1
            Case "DL" : strx = 2
            Case "BL" : strx = 3
            Case "AH" : strx = 4
            Case "CH" : strx = 5
            Case "DH" : strx = 6
            Case "BH" : strx = 7
        End Select
        Return strx
    End Function

    Public Function Reg16ToBin(ByVal Strs As String) As Byte
        Dim strx As String = ""
        Select Case Strs
            Case "AX" : strx = 0
            Case "CX" : strx = 1
            Case "DX" : strx = 2
            Case "BX" : strx = 3
            Case "SP" : strx = 4
            Case "BP" : strx = 5
            Case "SI" : strx = 6
            Case "DI" : strx = 7
        End Select
        Return strx
    End Function

    Public Function Reg32ToBin(ByVal Strs As String) As Byte
        Dim strx As String = ""
        Select Case Strs
            Case "EAX" : strx = 0
            Case "ECX" : strx = 1
            Case "EDX" : strx = 2
            Case "EBX" : strx = 3
            Case "ESP" : strx = 4
            Case "EBP" : strx = 5
            Case "ESI" : strx = 6
            Case "ESI" : strx = 7
        End Select
        'Stop
        Return strx
    End Function

    Public Function stToBin(ByVal Strs As String) As Byte
        Dim strx As String = ""
        Select Case Strs
            Case "ST(0)" : strx = 0
            Case "ST(1)" : strx = 1
            Case "ST(2)" : strx = 2
            Case "ST(3)" : strx = 3
            Case "ST(4)" : strx = 4
            Case "ST(5)" : strx = 5
            Case "ST(6)" : strx = 6
            Case "ST(7)" : strx = 7
        End Select
        'Stop
        Return strx
    End Function

    Public Function Reg64ToBin(ByVal Strs As String) As Byte
        Dim strx As String = ""
        Select Case Strs
            Case "MM0" : strx = 0
            Case "MM1" : strx = 1
            Case "MM2" : strx = 2
            Case "MM3" : strx = 3
            Case "MM4" : strx = 4
            Case "MM5" : strx = 5
            Case "MM6" : strx = 6
            Case "MM7" : strx = 7
        End Select
        Return strx
    End Function

    Public Function Reg128ToBin(ByVal Strs As String) As Byte
        Dim strx As String = ""
        Select Case Strs
            Case "XMM0" : strx = 0
            Case "XMM1" : strx = 1
            Case "XMM2" : strx = 2
            Case "XMM3" : strx = 3
            Case "XMM4" : strx = 4
            Case "XMM5" : strx = 5
            Case "XMM6" : strx = 6
            Case "XMM7" : strx = 7
        End Select
        Return strx
    End Function

    'reg_r/m con mod=11 quindi operazione reg, reg
    'r/m  |  reg w=0  |  reg w=1
    '000  |   al      |   ax
    '001  |   cl      |   cx
    '010  |   dl      |   dx
    '011  |   bl      |   bx
    '100  |   ah      |   sp
    '101  |   ch      |   bp
    '110  |   dh      |   si
    '111  |   bh      |   di

    'in caso di operazione reg, immediato si usano 
    'opcode  |  word_bit  |  reg_table
    'xxxx    |   8 o 16   |     xxx

    '    Mod_r/m_table     sreg:       [base + indice + spazziamento]
    'Mod  |  r/m  |  sreg predefinito  |  inidizzo effettivo
    '00   |  000  |        ds          |      [bx + si]
    '00   |  001  |        ds          |      [bx + di]
    '00   |  010  |        ss          |      [bp + si]
    '00   |  011  |        ss          |      [bp + di]
    '00   |  100  |        ds          |        [si]
    '00   |  101  |        ds          |        [di]
    '00   |  110  |        ds          |      [disp16]
    '00   |  111  |        ds          |        [bx]
    '01   |  000  |        ds          |  [bx + si + disp8]
    '01   |  001  |        ds          |  [bx + di + disp8]
    '01   |  010  |        ss          |  [bp + si + disp8]
    '01   |  011  |        ss          |  [bx + di + disp8]
    '01   |  100  |        ds          |    [si + disp8]
    '01   |  101  |        ds          |    [di + disp8]
    '01   |  110  |        ss          |    [bp + disp8]
    '01   |  111  |        ds          |    [bx + disp8]
    '10   |  000  |        ds          |  [bx + si + disp16]
    '10   |  001  |        ds          |  [bx + di + disp16]
    '10   |  010  |        ss          |  [bp + si + disp16]
    '10   |  011  |        ss          |  [bp + di + disp16]
    '10   |  100  |        ds          |    [si + disp16]
    '10   |  101  |        ds          |    [di + disp16]
    '10   |  110  |        ss          |    [bp + disp16]
    '10   |  111  |        ds          |    [bx + disp16]
    Public Function Mem16ToBin(ByVal Strx As String) As String
        Dim reg As Byte = 0
        Dim mode As Byte = 0
        Dim disp As String
        Dim tmp() As String, rm As Byte = 0
        Dim sign As String = "+"
        ' Stop
        sign = IIf(InStr(1, Strx, " - ") > 0, " - ", " + ")
        If sign = " - " Then
            Dim h() As String
            h = Split(Strx, " - ")
            Strx = h(0) & " + " & h(1)
        End If

        If InStr(1, Strx, " + ") > 0 Then
            tmp = Split(Strx, " + ") 'controlla se è di tipo esteso [offset + indice + segmentazione]
            disp = "" 'assegna l'offset ed inizializza i dati di default elementi 1
            If UBound(tmp) = 1 Then 'se ci sono 2 elementi conigurazioni [offset + indice] o [offset + segmentazione]
                If (tmp(0) = "BX" Or tmp(0) = "BP") And (tmp(1) = "SI" Or tmp(1) = "DI") Then 'controlla la configurazione [offset + indice]
                    rm = tmp(0) & "+" & tmp(1) 'assegna l'indice
                Else 'configurazione [offset + segmentazione]
                    If tmp(0) = "BX" Or tmp(0) = "BP" Or tmp(0) = "SI" Or tmp(0) = "DI" Then
                        rm = tmp(0)
                    Else 'configurazione non possibile
                        Console.WriteLine("Configurazione non possibile per tipo memoria " & Strx)
                        Return 0
                    End If
                    disp = tmp(1)
                End If

            ElseIf UBound(tmp) = 2 Then 'controlla se ci sono 3 elementi e la configurazione è di tipo esteso [offset + indice + segmentazione]
                If (tmp(0) = "bBXx" Or tmp(0) = "BP") And (tmp(1) = "SI" Or tmp(1) = "DI") And (Len(tmp(2)) = 2 Or Len(tmp(2)) = 4) Then
                    rm = tmp(0) & "+" & tmp(1)
                    disp = CShort(tmp(2)) 'estrappola ultimo elemento il displacement
                Else
                    Console.WriteLine("Configurazione non possibile per tipo memoria " & Strx)
                    Return 0
                End If
            End If
f1:
            If sign = "-" Then disp = -disp

            If disp = "" Then ' se non è presente il displacement 
                mode = 0 'mod è la prima sezione di indirizzamento senza segmentazione [offset + indice]
            ElseIf Len(disp) = 2 Then ' se il displacement è a 8bit 
                mode = 1 'mod è la seconda sezione di indirizzamento con segmentazione [offset + indice + segmentazione 8bit]
            ElseIf Len(disp) = 4 Then ' se il displacement è a 16bit
                mode = 2 'mod è la terza sezione di indirizzamento con segmentazione [offset + indice + segmentazione 16bit]
            End If
            Select Case rm 'controlla il tipo di indirizzamento senza displacement per assegnare il giusto valore del campo reg
                Case "BX+SI" : reg = 0
                Case "BX+DI" : reg = 1
                Case "BP+SI" : reg = 2
                Case "BP+DI" : reg = 3
                Case "SI" : reg = 4
                Case "DI" : reg = 5
                Case "BP"
                    reg = 6
                    If disp = 0 Then mode = 1 : disp = 0 'in caso di assenza di displacement caso speciale
                Case "BX" : reg = 7
            End Select
        Else 'configurazione [offset] o [indice] o [segmento]
            mode = 0
            Select Case Strx
                Case "SI" : reg = 4
                Case "DI" : reg = 5
                Case "BX" : reg = 7
                Case Else 'caso in cui sia un indirizzamento diretto [segmentazione 16bit]
                    reg = 5
                    disp = CShort(Strx)
            End Select
        End If
        Return EncodeModRM(mode, reg, rm) & IIf(disp <> 0, " _ " & disp, "")
    End Function

    Public Function Mem32ToBin(ByVal Strx As String) As String
        Dim mode As Byte = 0
        Dim rm As Byte = 0
        Dim disp As Integer = 0
        Dim sib As Byte = 0
        Dim s() As String
        Dim sign As String = "+"
        sign = IIf(InStr(1, Strx, "-") > 0, "-", "+")
        If sign = "-" Then
            Dim h() As String = Split(Strx, "-")
            Strx = h(0) & "+" & h(1)
        End If

        If InStr(1, Strx, "+") > 0 Then 'se più operatori
            s = Split(Strx, "+")
            If UBound(s) = 1 Then 'se 2 operatori
                If IsNumeric(s(1)) = True Then 'se l'ultimo operatore è di tipo displacement
                    If sign = "-" Then
                        disp = -CInt(s(1))
                    Else
                        disp = CInt(s(1))
                    End If
                    mode = If(disp >= -128 AndAlso disp <= 127, 1, 2)
                    Select Case s(0)
                        Case "EAX" : rm = 0
                        Case "ECX" : rm = 1
                        Case "EDX" : rm = 2
                        Case "EBX" : rm = 3
                        Case "EBP" : rm = 5
                        Case "ESI" : rm = 6
                        Case "EDI" : rm = 7
                        Case Else
                            mode = 0
                            rm = 4
                            sib = SibToBin(s(0), mode)
                    End Select
                Else
                    mode = 0
                    rm = 4
                    sib = SibToBin(s(0) & "+" & s(1), mode)
                End If
            ElseIf UBound(s) = 2 Then 'se sono 3 operatori

                If sign = "-" Then
                    disp = -CInt(s(2))
                Else
                    disp = CInt(s(2))
                End If
                mode = If(disp >= -128 AndAlso disp <= 127, 1, 2)
                rm = 4
                sib = SibToBin(s(0) & "+" & s(1), mode)
            End If
        Else
            mode = 0
            Select Case Strx
                Case "EAX" : rm = 0
                Case "ECX" : rm = 1
                Case "EDX" : rm = 2
                Case "EBX" : rm = 3
                Case "ESI" : rm = 6
                Case "EDI" : rm = 7
                Case Else
                    If IsNumeric(Strx) = True Then
                        rm = 5
                        disp = CInt(Strx)
                        mode = 2
                    End If
            End Select
        End If
        'Stop
        Return EncodeModRM(mode, 0, rm) & IIf(disp <> 0, " _ " & disp, "") & IIf(sib <> 0, " - " & sib, "")
    End Function

    Public Function SibToBin(ByVal Strx As String, Optional ByVal mode As Byte = 0) As Byte
        Dim scales As Byte = 0
        Dim index As Byte = 0
        Dim base As Byte = 0
        Dim s() As String, reg As String, reg1 As String

        If InStr(1, Strx, " * ") > 1 Then
            s = Split(Strx, " * ")
            Select Case s(1)
                Case "2"
                    scales = 1
                Case "4"
                    scales = 2
                Case "8"
                    scales = 3
            End Select
            Strx = Left(Strx, Len(Strx) - 2)
        Else
            scales = 0
        End If

        If mode = 0 And InStr(1, Strx, " + ") < 1 Then
            reg = Strx
            reg1 = "EBP"
        Else
            s = Split(Strx, " + ")
            reg = s(1)
            reg1 = s(0)
        End If

        Select Case reg
            Case "EAX"
                index = 0
            Case "ECX"
                index = 1
            Case "EDX"
                index = 2
            Case "EBX"
                index = 3
            Case "EBP"
                index = 5
            Case "ESI"
                index = 6
            Case "EDI"
                index = 7
        End Select
        Select Case reg1
            Case "EAX"
                base = 0
            Case "ECX"
                base = 1
            Case "EDX"
                base = 2
            Case "EBX"
                base = 3
            Case "ESP"
                base = 4
            Case "EBP"
                base = 5
            Case "ESI"
                base = 6
            Case "EDI"
                base = 7
        End Select
        Return EncodeSIB(scales, index, base)
    End Function

    Public Function MemToBin(ByVal Strx As String) As String
        If Is16BitAddressing(Strx) Then
            Return Mem16ToBin(Strx)
        Else
            Return Mem32ToBin(Strx)
        End If
    End Function

    Public Function Is16BitAddressing(ByVal Strx As String) As Boolean
        ' Lista di registri a 16 bit
        Dim reg16Bit() As String = {"AX", "BX", "CX", "DX", "SP", "BP", "SI", "DI"}

        ' Rimuovi spazi e converti in minuscolo per semplificare il confronto
        Strx = Strx.ToLower().Replace(" ", "")

        ' Controlla se la stringa contiene registri a 16 bit
        For Each reg In reg16Bit
            If Strx.Contains(reg) Then
                ' Se non contiene registri a 32 bit, assume che sia 16 bit
                If Not Strx.Contains("EAX") AndAlso Not Strx.Contains("EBX") AndAlso
       Not Strx.Contains("ECX") AndAlso Not Strx.Contains("EDX") AndAlso
       Not Strx.Contains("ESP") AndAlso Not Strx.Contains("EBP") AndAlso
       Not Strx.Contains("ESI") AndAlso Not Strx.Contains("EDI") Then
                    Return True
                End If
            End If
        Next


        ' Altrimenti, assume che sia 32 bit
        Return False
    End Function

    Public Function CCToBin(ByVal Strs As String) As Byte
        Dim strx As Byte
        Strs = LCase(Strs)
        If Strs = "o" Then
            strx = 0
        ElseIf Strs = "no" Then
            strx = 1
        ElseIf Strs = "b" Or Strs = "nae" Then
            strx = 2
        ElseIf Strs = "nb" Or Strs = "ae" Then
            strx = 3
        ElseIf Strs = "z" Or Strs = "e" Then
            strx = 4
        ElseIf Strs = "nz" Or Strs = "ne" Then
            strx = 5
        ElseIf Strs = "na" Or Strs = "be" Then
            strx = 6
        ElseIf Strs = "a" Or Strs = "nbe" Then
            strx = 7
        ElseIf Strs = "s" Then
            strx = 8
        ElseIf Strs = "ns" Then
            strx = 9
        ElseIf Strs = "p" Or Strs = "pe" Then
            strx = 10
        ElseIf Strs = "np" Or Strs = "po" Then
            strx = 11
        ElseIf Strs = "l" Or Strs = "nge" Then
            strx = 12
        ElseIf Strs = "nl" Or Strs = "ge" Then
            strx = 13
        ElseIf Strs = "ng" Or Strs = "le" Then
            strx = 14
        ElseIf Strs = "g" Or Strs = "nle" Then
            strx = 15
        End If
        Return strx
    End Function

    Public Function CCcToBin(ByVal Strs As String) As Byte
        Dim strx As Byte = ""
        Strs = LCase(Strs)
        If Strs = "b" Then
            strx = 0
        ElseIf Strs = "nb" Then
            strx = 8
        ElseIf Strs = "e" Then
            strx = 1
        ElseIf Strs = "ne" Then
            strx = 9
        ElseIf Strs = "be" Then
            strx = 2
        ElseIf Strs = "nbe" Then
            strx = 10
        ElseIf Strs = "u" Then
            strx = 3
        ElseIf Strs = "nu" Then
            strx = 11
        End If
        Return strx
    End Function

End Module
