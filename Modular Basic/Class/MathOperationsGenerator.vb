Public Class MathOperationsGenerator

    Public Sub New()
        GenerateBasicOperations()
        GenerateTrigOperations()
        GenerateAdvancedOperations()
    End Sub

    Public Sub GenerateOperation(op As String, dest As String, src1 As String, src2 As String, type As String)
        ' Setup parametri base
        _CodeGen.Append($"    mov edi, {src1}")     ' Primo operando
        _CodeGen.Append($"    mov eax, {dest}")     ' Destinazione
        If src2 IsNot Nothing Then
            _CodeGen.Append($"    mov esi, {src2}") ' Secondo operando
        End If

        Select Case op
            Case "+", "-", "*", "/"
                If type = "Float" Then
                    _CodeGen.Append($"    call Float{op}")
                Else
                    _CodeGen.Append($"    call Int{op}")
                End If
            Case "^"
                _CodeGen.Append("    call Power")
            Case "Sqrt"
                _CodeGen.Append("    call Sqrt")
            Case "Sin", "Cos", "Tan"
                _CodeGen.Append($"    call {op}")
            Case "Abs"
                If type = "Float" Then
                    _CodeGen.Append("    call FloatAbs")
                Else
                    _CodeGen.Append("    call IntAbs")
                End If
            Case "Mod"
                If type = "Float" Then
                    _CodeGen.Append("    call FloatMod")
                Else
                    _CodeGen.Append("    call IntMod")
                End If
        End Select
    End Sub

    Private Sub GenerateBasicOperations()
        ' Operazioni Float
        _LibGen.Append("Float+:")
        _LibGen.Append("    fld dword [edi]")      ' Primo operando
        _LibGen.Append("    fadd dword [esi]")     ' Somma secondo operando
        _LibGen.Append("    fstp dword [eax]")     ' Salva risultato
        _LibGen.Append("    ret")

        _LibGen.Append("Float-:")
        _LibGen.Append("    fld dword [edi]")
        _LibGen.Append("    fsub dword [esi]")
        _LibGen.Append("    fstp dword [eax]")
        _LibGen.Append("    ret")

        _LibGen.Append("Float*:")
        _LibGen.Append("    fld dword [edi]")
        _LibGen.Append("    fmul dword [esi]")
        _LibGen.Append("    fstp dword [eax]")
        _LibGen.Append("    ret")

        _LibGen.Append("Float/:")
        _LibGen.Append("    push eax")
        _LibGen.Append("    fld dword [esi]")      ' Controllo divisione per zero
        _LibGen.Append("    ftst")
        _LibGen.Append("    fstsw ax")
        _LibGen.Append("    fwait")
        _LibGen.Append("    sahf")
        _LibGen.Append("    pop eax")
        _LibGen.Append("    jz division_by_zero")
        _LibGen.Append("    fld dword [edi]")
        _LibGen.Append("    fdiv dword [esi]")
        _LibGen.Append("    fstp dword [eax]")
        _LibGen.Append("    ret")

        ' Operazioni Int
        _LibGen.Append("Int+:")
        _LibGen.Append("    mov ecx, [edi]")
        _LibGen.Append("    add ecx, [esi]")
        _LibGen.Append("    jo overflow_error")
        _LibGen.Append("    mov [eax], ecx")
        _LibGen.Append("    ret")

        _LibGen.Append("Int-:")
        _LibGen.Append("    mov ecx, [edi]")
        _LibGen.Append("    sub ecx, [esi]")
        _LibGen.Append("    jo overflow_error")
        _LibGen.Append("    mov [eax], ecx")
        _LibGen.Append("    ret")

        _LibGen.Append("Int*:")
        _LibGen.Append("    mov ecx, [edi]")
        _LibGen.Append("    imul ecx, [esi]")
        _LibGen.Append("    jo overflow_error")
        _LibGen.Append("    mov [eax], ecx")
        _LibGen.Append("    ret")

        _LibGen.Append("Int/:")
        _LibGen.Append("    mov ecx, [esi]")
        _LibGen.Append("    test ecx, ecx")
        _LibGen.Append("    jz division_by_zero")
        _LibGen.Append("    mov eax, [edi]")
        _LibGen.Append("    cdq")
        _LibGen.Append("    idiv ecx")
        _LibGen.Append("    mov [eax], eax")
        _LibGen.Append("    ret")
    End Sub

    Private Sub GenerateTrigOperations()

        ' Funzioni trigonometriche
        _LibGen.Append("Sin:")
        _LibGen.Append("    fld dword [edi]")
        _LibGen.Append("    fsin")
        _LibGen.Append("    fstp dword [eax]")
        _LibGen.Append("    ret")

        _LibGen.Append("Cos:")
        _LibGen.Append("    fld dword [edi]")
        _LibGen.Append("    fcos")
        _LibGen.Append("    fstp dword [eax]")
        _LibGen.Append("    ret")

        _LibGen.Append("Tan:")
        _LibGen.Append("    fld dword [edi]")
        _LibGen.Append("    fptan")
        _LibGen.Append("    fstp st(0)")
        _LibGen.Append("    fstp dword [eax]")
        _LibGen.Append("    ret")
    End Sub

    Private Sub GenerateAdvancedOperations()
        ' Potenza
        _LibGen.Append("Power:")
        _LibGen.Append("    push ebx")
        _LibGen.Append("    push ecx")

        _LibGen.Append("    fld dword [edi]")      ' Carica base
        _LibGen.Append("    ftst")                 ' Test se base = 0
        _LibGen.Append("    fstsw ax")
        _LibGen.Append("    fwait")
        _LibGen.Append("    sahf")
        _LibGen.Append("    jz .zero_base")       ' Gestione caso base zero

        _LibGen.Append("    fld dword [esi]")     ' Carica esponente
        _LibGen.Append("    fxch")                ' Scambia ST(0) e ST(1)
        _LibGen.Append("    fyl2x")               ' ST(0) = ST(1) * log2(ST(0))
        _LibGen.Append("    fld st(0)")           ' Duplica il risultato
        _LibGen.Append("    frndint")             ' Arrotonda all'intero
        _LibGen.Append("    fsubr st(1), st(0)")  ' ST(1) = ST(1) - ST(0)
        _LibGen.Append("    fxch")                ' Scambia ST(0) e ST(1)
        _LibGen.Append("    f2xm1")               ' ST(0) = 2^ST(0) - 1
        _LibGen.Append("    fld1")
        _LibGen.Append("    faddp")               ' Aggiungi 1
        _LibGen.Append("    fscale")              ' Scale by ST(1)
        _LibGen.Append("    fstp st(1)")          ' Rimuovi esponente
        _LibGen.Append("    jmp .done")

        _LibGen.Append(".zero_base:")
        _LibGen.Append("    fld dword [esi]")     ' Controlla esponente > 0
        _LibGen.Append("    ftst")
        _LibGen.Append("    fstsw ax")
        _LibGen.Append("    fwait")
        _LibGen.Append("    sahf")
        _LibGen.Append("    jbe domain_error")    ' Se esponente <= 0, errore
        _LibGen.Append("    fldz")                ' Risultato = 0

        _LibGen.Append(".done:")
        _LibGen.Append("    fstp dword [eax]")    ' Salva risultato
        _LibGen.Append("    pop ecx")
        _LibGen.Append("    pop ebx")
        _LibGen.Append("    ret")

        ' Radice quadrata
        _LibGen.Append("Sqrt:")
        _LibGen.Append("    fld dword [edi]")     ' Carica valore
        _LibGen.Append("    ftst")                ' Test se negativo
        _LibGen.Append("    fstsw ax")
        _LibGen.Append("    fwait")
        _LibGen.Append("    sahf")
        _LibGen.Append("    jb domain_error")     ' Errore se negativo

        _LibGen.Append("    fsqrt")               ' Calcola radice quadrata
        _LibGen.Append("    fstp dword [eax]")    ' Salva risultato
        _LibGen.Append("    ret")

        ' Modulo
        _LibGen.Append("FloatMod:")
        _LibGen.Append("    fld dword [edi]")
        _LibGen.Append("    fld dword [esi]")
        _LibGen.Append("    ftst")                ' Test se divisore = 0
        _LibGen.Append("    fstsw ax")
        _LibGen.Append("    fwait")
        _LibGen.Append("    sahf")
        _LibGen.Append("    jz division_by_zero")
        _LibGen.Append("    fprem")               ' Calcola resto
        _LibGen.Append("    fstp st(1)")          ' Rimuovi divisore
        _LibGen.Append("    fstp dword [eax]")
        _LibGen.Append("    ret")

        _LibGen.Append("IntMod:")
        _LibGen.Append("    mov ecx, [esi]")
        _LibGen.Append("    test ecx, ecx")
        _LibGen.Append("    jz division_by_zero")
        _LibGen.Append("    mov eax, [edi]")
        _LibGen.Append("    cdq")                 ' Estendi segno in EDX
        _LibGen.Append("    idiv ecx")            ' EDX contiene il resto
        _LibGen.Append("    mov [eax], edx")
        _LibGen.Append("    ret")

        ' Valore assoluto
        _LibGen.Append("FloatAbs:")
        _LibGen.Append("    fld dword [edi]")
        _LibGen.Append("    fabs")
        _LibGen.Append("    fstp dword [eax]")
        _LibGen.Append("    ret")

        _LibGen.Append("IntAbs:")
        _LibGen.Append("    mov ecx, [edi]")
        _LibGen.Append("    test ecx, ecx")
        _LibGen.Append("    jns .done")
        _LibGen.Append("    neg ecx")
        _LibGen.Append(".done:")
        _LibGen.Append("    mov [eax], ecx")
        _LibGen.Append("    ret")
    End Sub

End Class