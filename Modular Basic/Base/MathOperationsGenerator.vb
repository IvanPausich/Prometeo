Public Class MathOperationsGenerator

    Public Sub New()
        GenerateHelperProcedures()
    End Sub

    Private Sub GenerateHelperProcedures()
        GenerateBasicOperations()
        GenerateTrigOperations()
        GenerateAdvancedOperations()
    End Sub

    Public Sub GenerateOperation(op As String, dest As String, src1 As String, src2 As String, type As String)
        ' Setup parametri base
        _CodeGen.AppendLine($"    mov edi, {src1}")     ' Primo operando
        _CodeGen.AppendLine($"    mov eax, {dest}")     ' Destinazione
        If src2 IsNot Nothing Then
            _CodeGen.AppendLine($"    mov esi, {src2}") ' Secondo operando
        End If

        Select Case op
            Case "+", "-", "*", "/"
                If type = "Float" Then
                    _CodeGen.AppendLine($"    call Float{op}")
                Else
                    _CodeGen.AppendLine($"    call Int{op}")
                End If
            Case "^"
                _CodeGen.AppendLine("    call Power")
            Case "Sqrt"
                _CodeGen.AppendLine("    call Sqrt")
            Case "Sin", "Cos", "Tan"
                _CodeGen.AppendLine($"    call {op}")
            Case "Abs"
                If type = "Float" Then
                    _CodeGen.AppendLine("    call FloatAbs")
                Else
                    _CodeGen.AppendLine("    call IntAbs")
                End If
            Case "Mod"
                If type = "Float" Then
                    _CodeGen.AppendLine("    call FloatMod")
                Else
                    _CodeGen.AppendLine("    call IntMod")
                End If
        End Select
    End Sub

    Private Sub GenerateBasicOperations()
        ' Operazioni Float
        _LibGen.appendline("Float+:")
        _LibGen.appendline("    fld dword [edi]")      ' Primo operando
        _LibGen.appendline("    fadd dword [esi]")     ' Somma secondo operando
        _LibGen.appendline("    fstp dword [eax]")     ' Salva risultato
        _LibGen.appendline("    ret")

        _LibGen.appendline("Float-:")
        _LibGen.appendline("    fld dword [edi]")
        _LibGen.appendline("    fsub dword [esi]")
        _LibGen.appendline("    fstp dword [eax]")
        _LibGen.appendline("    ret")

        _LibGen.appendline("Float*:")
        _LibGen.appendline("    fld dword [edi]")
        _LibGen.appendline("    fmul dword [esi]")
        _LibGen.appendline("    fstp dword [eax]")
        _LibGen.appendline("    ret")

        _LibGen.appendline("Float/:")
        _LibGen.appendline("    push eax")
        _LibGen.appendline("    fld dword [esi]")      ' Controllo divisione per zero
        _LibGen.appendline("    ftst")
        _LibGen.appendline("    fstsw ax")
        _LibGen.appendline("    fwait")
        _LibGen.appendline("    sahf")
        _LibGen.appendline("    pop eax")
        _LibGen.appendline("    jz division_by_zero")
        _LibGen.appendline("    fld dword [edi]")
        _LibGen.appendline("    fdiv dword [esi]")
        _LibGen.appendline("    fstp dword [eax]")
        _LibGen.appendline("    ret")

        ' Operazioni Int
        _LibGen.appendline("Int+:")
        _LibGen.appendline("    mov ecx, [edi]")
        _LibGen.appendline("    add ecx, [esi]")
        _LibGen.appendline("    jo overflow_error")
        _LibGen.appendline("    mov [eax], ecx")
        _LibGen.appendline("    ret")

        _LibGen.appendline("Int-:")
        _LibGen.appendline("    mov ecx, [edi]")
        _LibGen.appendline("    sub ecx, [esi]")
        _LibGen.appendline("    jo overflow_error")
        _LibGen.appendline("    mov [eax], ecx")
        _LibGen.appendline("    ret")

        _LibGen.appendline("Int*:")
        _LibGen.appendline("    mov ecx, [edi]")
        _LibGen.appendline("    imul ecx, [esi]")
        _LibGen.appendline("    jo overflow_error")
        _LibGen.appendline("    mov [eax], ecx")
        _LibGen.appendline("    ret")

        _LibGen.appendline("Int/:")
        _LibGen.appendline("    mov ecx, [esi]")
        _LibGen.appendline("    test ecx, ecx")
        _LibGen.appendline("    jz division_by_zero")
        _LibGen.appendline("    mov eax, [edi]")
        _LibGen.appendline("    cdq")
        _LibGen.appendline("    idiv ecx")
        _LibGen.appendline("    mov [eax], eax")
        _LibGen.appendline("    ret")
    End Sub

    Private Sub GenerateTrigOperations()

        ' Funzioni trigonometriche
        _LibGen.appendline("Sin:")
        _LibGen.appendline("    fld dword [edi]")
        _LibGen.appendline("    fsin")
        _LibGen.appendline("    fstp dword [eax]")
        _LibGen.appendline("    ret")

        _LibGen.appendline("Cos:")
        _LibGen.appendline("    fld dword [edi]")
        _LibGen.appendline("    fcos")
        _LibGen.appendline("    fstp dword [eax]")
        _LibGen.appendline("    ret")

        _LibGen.appendline("Tan:")
        _LibGen.appendline("    fld dword [edi]")
        _LibGen.appendline("    fptan")
        _LibGen.appendline("    fstp st(0)")
        _LibGen.appendline("    fstp dword [eax]")
        _LibGen.appendline("    ret")
    End Sub

    Private Sub GenerateAdvancedOperations()
        ' Potenza
        _LibGen.appendline("Power:")
        _LibGen.appendline("    push ebx")
        _LibGen.appendline("    push ecx")

        _LibGen.appendline("    fld dword [edi]")      ' Carica base
        _LibGen.appendline("    ftst")                 ' Test se base = 0
        _LibGen.appendline("    fstsw ax")
        _LibGen.appendline("    fwait")
        _LibGen.appendline("    sahf")
        _LibGen.appendline("    jz .zero_base")       ' Gestione caso base zero

        _LibGen.appendline("    fld dword [esi]")     ' Carica esponente
        _LibGen.appendline("    fxch")                ' Scambia ST(0) e ST(1)
        _LibGen.appendline("    fyl2x")               ' ST(0) = ST(1) * log2(ST(0))
        _LibGen.appendline("    fld st(0)")           ' Duplica il risultato
        _LibGen.appendline("    frndint")             ' Arrotonda all'intero
        _LibGen.appendline("    fsubr st(1), st(0)")  ' ST(1) = ST(1) - ST(0)
        _LibGen.appendline("    fxch")                ' Scambia ST(0) e ST(1)
        _LibGen.appendline("    f2xm1")               ' ST(0) = 2^ST(0) - 1
        _LibGen.appendline("    fld1")
        _LibGen.appendline("    faddp")               ' Aggiungi 1
        _LibGen.appendline("    fscale")              ' Scale by ST(1)
        _LibGen.appendline("    fstp st(1)")          ' Rimuovi esponente
        _LibGen.appendline("    jmp .done")

        _LibGen.appendline(".zero_base:")
        _LibGen.appendline("    fld dword [esi]")     ' Controlla esponente > 0
        _LibGen.appendline("    ftst")
        _LibGen.appendline("    fstsw ax")
        _LibGen.appendline("    fwait")
        _LibGen.appendline("    sahf")
        _LibGen.appendline("    jbe domain_error")    ' Se esponente <= 0, errore
        _LibGen.appendline("    fldz")                ' Risultato = 0

        _LibGen.appendline(".done:")
        _LibGen.appendline("    fstp dword [eax]")    ' Salva risultato
        _LibGen.appendline("    pop ecx")
        _LibGen.appendline("    pop ebx")
        _LibGen.appendline("    ret")

        ' Radice quadrata
        _LibGen.appendline("Sqrt:")
        _LibGen.appendline("    fld dword [edi]")     ' Carica valore
        _LibGen.appendline("    ftst")                ' Test se negativo
        _LibGen.appendline("    fstsw ax")
        _LibGen.appendline("    fwait")
        _LibGen.appendline("    sahf")
        _LibGen.appendline("    jb domain_error")     ' Errore se negativo

        _LibGen.appendline("    fsqrt")               ' Calcola radice quadrata
        _LibGen.appendline("    fstp dword [eax]")    ' Salva risultato
        _LibGen.appendline("    ret")

        ' Modulo
        _LibGen.appendline("FloatMod:")
        _LibGen.appendline("    fld dword [edi]")
        _LibGen.appendline("    fld dword [esi]")
        _LibGen.appendline("    ftst")                ' Test se divisore = 0
        _LibGen.appendline("    fstsw ax")
        _LibGen.appendline("    fwait")
        _LibGen.appendline("    sahf")
        _LibGen.appendline("    jz division_by_zero")
        _LibGen.appendline("    fprem")               ' Calcola resto
        _LibGen.appendline("    fstp st(1)")          ' Rimuovi divisore
        _LibGen.appendline("    fstp dword [eax]")
        _LibGen.appendline("    ret")

        _LibGen.appendline("IntMod:")
        _LibGen.appendline("    mov ecx, [esi]")
        _LibGen.appendline("    test ecx, ecx")
        _LibGen.appendline("    jz division_by_zero")
        _LibGen.appendline("    mov eax, [edi]")
        _LibGen.appendline("    cdq")                 ' Estendi segno in EDX
        _LibGen.appendline("    idiv ecx")            ' EDX contiene il resto
        _LibGen.appendline("    mov [eax], edx")
        _LibGen.appendline("    ret")

        ' Valore assoluto
        _LibGen.appendline("FloatAbs:")
        _LibGen.appendline("    fld dword [edi]")
        _LibGen.appendline("    fabs")
        _LibGen.appendline("    fstp dword [eax]")
        _LibGen.appendline("    ret")

        _LibGen.appendline("IntAbs:")
        _LibGen.appendline("    mov ecx, [edi]")
        _LibGen.appendline("    test ecx, ecx")
        _LibGen.appendline("    jns .done")
        _LibGen.appendline("    neg ecx")
        _LibGen.appendline(".done:")
        _LibGen.appendline("    mov [eax], ecx")
        _LibGen.appendline("    ret")
    End Sub

End Class