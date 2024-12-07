' Classe per gestire le stringhe
Public Class StringHandler

    Public Sub New()
        ' Genera le procedure di supporto
        GenerateHelperProcedures()
    End Sub

    Private Sub GenerateHelperProcedures()
        GenerateStringCopy()
        GenerateStringConcat()
        GenerateStringLength()
        GenerateStringCompare()
        GenerateStringUCase()
        GenerateStringLCase()
        GenerateStringRight()
        GenerateStringLeft()
        GenerateStringMid()
        GenerateStringTrim()       ' Nuova funzione
        GenerateStringReplace()    ' Nuova funzione
        GenerateStringIndexOf()    ' Nuova funzione
    End Sub

    Public Sub GenerateStringOperation(operation As String, dest As String, src As String,
                                       Optional length As String = Nothing,
                                       Optional start As String = Nothing,
                                       Optional replacement As String = Nothing)
        ' Controllo parametri nulli
        _CodeGen.Append("    ; Controllo parametri")
        _CodeGen.Append($"    cmp {src}, 0")
        _CodeGen.Append("    je null_string_error")
        _CodeGen.Append($"    cmp {dest}, 0")
        _CodeGen.Append("    je null_string_error")

        Select Case operation
            Case "COPY"
                ' Passa i parametri e chiama la funzione per copiare una stringa
                _CodeGen.Append($"    mov esi, {src}")
                _CodeGen.Append($"    mov edi, {dest}")
                If length IsNot Nothing Then
                    _CodeGen.Append($"    mov ecx, {length}")
                Else
                    ' Calcola lunghezze
                    _CodeGen.Append("    push esi")              ' Salva puntatori
                    _CodeGen.Append("    push edi")
                    _CodeGen.Append($"    mov edi, [{src}]")
                    _CodeGen.Append("    call StringLength")
                    _CodeGen.Append("    pop edi")
                    _CodeGen.Append("    pop esi")              ' Salva puntatori
                End If
                _CodeGen.Append($"    call StringCopy")
            Case "&"
                ' Passa i parametri e chiama la funzione per concatenare due stringhe
                _CodeGen.Append($"    mov esi, {src}")
                _CodeGen.Append($"    mov edi, {dest}")
                _CodeGen.Append($"    call StringConcat")
            Case "LEN"
                ' Passa i parametri e chiama la funzione per ottenere la lunghezza di una stringa
                _CodeGen.Append($"    mov edi, {src}")
                _CodeGen.Append($"    call StringLength")
                _CodeGen.Append($"    mov [{dest}], ecx")
            Case "COMPARE"
                ' Passa i parametri e chiama la funzione per confrontare due stringhe
                _CodeGen.Append($"    mov esi, {dest}")
                _CodeGen.Append($"    mov edi, {src}")
                _CodeGen.Append($"    call StringCompare")
                _CodeGen.Append($"    mov [{dest}], eax")
            Case "UCASE"
                ' Passa i parametri e chiama la funzione per convertire una stringa in maiuscolo
                _CodeGen.Append($"    mov esi, {src}")
                _CodeGen.Append($"    mov edi, {dest}")
                _CodeGen.Append($"    call StringUCase")
            Case "LCASE"
                ' Passa i parametri e chiama la funzione per convertire una stringa in minuscolo
                _CodeGen.Append($"    mov esi, {src}")
                _CodeGen.Append($"    mov edi, {dest}")
                _CodeGen.Append($"    call StringLCase")
            Case "RIGHT"
                ' Passa i parametri e chiama la funzione per ottenere i caratteri a destra di una stringa
                _CodeGen.Append($"    mov esi, {src}")
                _CodeGen.Append($"    mov edi, {dest}")
                _CodeGen.Append($"    mov ecx, {length}")
                _CodeGen.Append($"    call StringRight")
            Case "LEFT"
                ' Passa i parametri e chiama la funzione per ottenere i caratteri a sinistra di una stringa
                _CodeGen.Append($"    mov esi, {src}")
                _CodeGen.Append($"    mov edi, {dest}")
                _CodeGen.Append($"    mov ecx, {length}")
                _CodeGen.Append($"    call StringLeft")
            Case "MID"
                ' Passa i parametri e chiama la funzione per ottenere i caratteri centrali di una stringa
                _CodeGen.Append($"    mov esi, {src}")
                _CodeGen.Append($"    mov edi, {dest}")
                _CodeGen.Append($"    mov ecx, {start}")
                _CodeGen.Append($"    mov edx, {length}")
                _CodeGen.Append($"    call StringMid")
            Case "TRIM"
                _CodeGen.Append($"    mov esi, {src}")
                _CodeGen.Append($"    mov edi, {dest}")
                _CodeGen.Append("    call StringTrim")

            Case "REPLACE"
                _CodeGen.Append($"    mov esi, {src}")           ' Stringa originale
                _CodeGen.Append($"    mov edi, {dest}")          ' Destinazione
                _CodeGen.Append($"    mov edx, {start}")         ' Stringa da cercare
                _CodeGen.Append($"    mov ebx, {replacement}")   ' Stringa sostitutiva
                _CodeGen.Append("    call StringReplace")

            Case "INDEXOF"
                _CodeGen.Append($"    mov esi, {src}")          ' Stringa in cui cercare
                _CodeGen.Append($"    mov edi, {start}")        ' Sottostringa da trovare
                _CodeGen.Append($"    mov [{dest}], eax")       ' Salva risultato
                _CodeGen.Append("    call StringIndexOf")
        End Select

    End Sub

    Private Sub GenerateStringTrim()
        _LibGen.Append("StringTrim:")
        ' Trova inizio (salta spazi iniziali)
        _LibGen.Append("    push esi")              ' Salva puntatore originale
        _LibGen.Append(".skip_spaces_start:")
        _LibGen.Append("    lodsb")
        _LibGen.Append("    cmp al, ' '")
        _LibGen.Append("    je .skip_spaces_start")
        _LibGen.Append("    cmp al, 9")            ' Tab
        _LibGen.Append("    je .skip_spaces_start")
        _LibGen.Append("    dec esi")              ' Torna all'ultimo carattere non spazio

        ' Trova fine (salta spazi finali)
        _LibGen.Append("    mov edx, esi")         ' Salva inizio stringa pulita
        _LibGen.Append("    call StringLength")
        _LibGen.Append("    add esi, ecx")         ' Vai alla fine
        _LibGen.Append("    dec esi")

        _LibGen.Append(".skip_spaces_end:")
        _LibGen.Append("    mov al, [esi]")
        _LibGen.Append("    cmp al, ' '")
        _LibGen.Append("    je .next_end")
        _LibGen.Append("    cmp al, 9")
        _LibGen.Append("    jne .copy_trim")

        _LibGen.Append(".next_end:")
        _LibGen.Append("    dec esi")
        _LibGen.Append("    jmp .skip_spaces_end")

        ' Copia la stringa pulita
        _LibGen.Append(".copy_trim:")
        _LibGen.Append("    mov ecx, esi")
        _LibGen.Append("    sub ecx, edx")
        _LibGen.Append("    inc ecx")              ' Include l'ultimo carattere
        _LibGen.Append("    mov esi, edx")
        _LibGen.Append("    rep movsb")
        _LibGen.Append("    mov byte [edi], 0")    ' Termina la stringa
        _LibGen.Append("    pop esi")              ' Ripristina puntatore
        _LibGen.Append("    ret")
    End Sub

    Private Sub GenerateStringReplace()
        _LibGen.Append("StringReplace:")
        ' esi = stringa originale
        ' edi = destinazione
        ' edx = stringa da cercare
        ' ebx = stringa sostitutiva

        _LibGen.Append("    push esi")              ' Salva puntatori
        _LibGen.Append("    push edi")

        ' Calcola lunghezze
        _LibGen.Append("    push edx")
        _LibGen.Append("    mov edi, edx")
        _LibGen.Append("    call StringLength")
        _LibGen.Append("    mov edx, ecx")          ' edx = lunghezza da cercare
        _LibGen.Append("    pop edi")

        _LibGen.Append(".search_loop:")
        _LibGen.Append("    mov ecx, edx")          ' Lunghezza da confrontare
        _LibGen.Append("    push esi")
        _LibGen.Append("    push edi")
        _LibGen.Append("    repe cmpsb")            ' Confronta
        _LibGen.Append("    pop edi")
        _LibGen.Append("    pop esi")

        _LibGen.Append("    je .replace_found")     ' Se trovato, sostituisci

        _LibGen.Append("    lodsb")                 ' Altrimenti copia carattere
        _LibGen.Append("    stosb")
        _LibGen.Append("    test al, al")
        _LibGen.Append("    jnz .search_loop")
        _LibGen.Append("    jmp .replace_done")

        _LibGen.Append(".replace_found:")
        _LibGen.Append("    push esi")
        _LibGen.Append("    mov esi, ebx")          ' Copia stringa sostitutiva
        _LibGen.Append("    call StringLength")
        _LibGen.Append("    rep movsb")
        _LibGen.Append("    pop esi")
        _LibGen.Append("    add esi, edx")          ' Salta stringa originale
        _LibGen.Append("    jmp .search_loop")

        _LibGen.Append(".replace_done:")
        _LibGen.Append("    pop edi")
        _LibGen.Append("    pop esi")
        _LibGen.Append("    ret")
    End Sub

    Private Sub GenerateStringIndexOf()
        _LibGen.Append("StringIndexOf:")
        ' esi = stringa in cui cercare
        ' edi = sottostringa da trovare

        _LibGen.Append("    push esi")
        _LibGen.Append("    push edi")

        ' Calcola lunghezza sottostringa
        _LibGen.Append("    push esi")
        _LibGen.Append("    mov esi, edi")
        _LibGen.Append("    call StringLength")
        _LibGen.Append("    mov edx, ecx")         ' edx = lunghezza da cercare
        _LibGen.Append("    pop esi")

        _LibGen.Append("    xor ebx, ebx")         ' ebx = posizione corrente

        _LibGen.Append(".search_loop:")
        _LibGen.Append("    mov ecx, edx")         ' Lunghezza da confrontare
        _LibGen.Append("    push esi")
        _LibGen.Append("    push edi")
        _LibGen.Append("    repe cmpsb")           ' Confronta
        _LibGen.Append("    pop edi")
        _LibGen.Append("    pop esi")

        _LibGen.Append("    je .found")            ' Se trovato

        _LibGen.Append("    inc ebx")
        _LibGen.Append("    inc esi")
        _LibGen.Append("    cmp byte [esi], 0")
        _LibGen.Append("    jne .search_loop")

        _LibGen.Append("    mov ebx, -1")          ' Non trovato

        _LibGen.Append(".found:")
        _LibGen.Append("    mov eax, ebx")         ' Ritorna posizione
        _LibGen.Append("    pop edi")
        _LibGen.Append("    pop esi")
        _LibGen.Append("    ret")
    End Sub

    Private Sub GenerateStringUCase()
        _LibGen.Append($"Ucase:")
        ' Imposta ecx a 0xFFFFFFFF per scorrere l'intera stringa
        _LibGen.Append("    mov ecx, 0xFFFFFFFF")
        ' Etichetta per l'inizio del ciclo
        _LibGen.Append(".loop:")
        ' Carica un byte da esi in al e incrementa esi
        _LibGen.Append("    lodsb")
        ' Confronta al con 0 (fine stringa)
        _LibGen.Append("    cmp al, 0")
        ' Se al è 0, salta a .done
        _LibGen.Append("    je .done")
        ' Confronta al con 'a'
        _LibGen.Append("    cmp al, 'a'")
        ' Se al è minore di 'a', salta a .skip
        _LibGen.Append("    jb .skip")
        ' Confronta al con 'z'
        _LibGen.Append("    cmp al, 'z'")
        ' Se al è maggiore di 'z', salta a .skip
        _LibGen.Append("    ja .skip")
        ' Sottrae 32 da al per convertire in maiuscolo
        _LibGen.Append("    sub al, 32")
        ' Etichetta .skip
        _LibGen.Append(".skip:")
        ' Memorizza al in edi e incrementa edi
        _LibGen.Append("    stosb")
        ' Salta a .loop per continuare il ciclo
        _LibGen.Append("    jmp .loop")
        ' Etichetta .done per la fine del ciclo
        _LibGen.Append(".done:")
        _LibGen.Append($"ret")
    End Sub

    Private Sub GenerateStringLCase()
        _LibGen.Append($"Lcase:")
        ' Imposta ecx a 0xFFFFFFFF per scorrere l'intera stringa
        _LibGen.Append("    mov ecx, 0xFFFFFFFF")
        ' Etichetta per l'inizio del ciclo
        _LibGen.Append(".loop:")
        ' Carica un byte da esi in al e incrementa esi
        _LibGen.Append("    lodsb")
        ' Confronta al con 0 (fine stringa)
        _LibGen.Append("    cmp al, 0")
        ' Se al è 0, salta a .done
        _LibGen.Append("    je .done")
        ' Confronta al con 'A'
        _LibGen.Append("    cmp al, 'A'")
        ' Se al è minore di 'A', salta a .skip
        _LibGen.Append("    jb .skip")
        ' Confronta al con 'Z'
        _LibGen.Append("    cmp al, 'Z'")
        ' Se al è maggiore di 'Z', salta a .skip
        _LibGen.Append("    ja .skip")
        ' Aggiunge 32 ad al per convertire in minuscolo
        _LibGen.Append("    add al, 32")
        ' Etichetta .skip
        _LibGen.Append(".skip:")
        ' Memorizza al in edi e incrementa edi
        _LibGen.Append("    stosb")
        ' Salta a .loop per continuare il ciclo
        _LibGen.Append("    jmp .loop")
        ' Etichetta .done per la fine del ciclo
        _LibGen.Append(".done:")
        _LibGen.Append($"ret")
    End Sub

    Private Sub GenerateStringRight()
        _LibGen.Append($"StringRight:")
        ' sposta in eax il numero di caratteri da restituire
        _LibGen.Append("    mov eax, ecx")

        ' Salva i registri utilizzati
        _LibGen.Append("    push esi")
        _LibGen.Append("    push edi")

        ' Calcola la lunghezza della stringa di origine
        _LibGen.Append($"    mov edi, esi")
        _LibGen.Append("    call StringLength")

        ' Ripristina esi e edi
        _LibGen.Append("    pop edi")
        _LibGen.Append("    pop esi")

        ' Calcola la posizione di inizio
        _LibGen.Append("    sub ecx, eax")
        _LibGen.Append("    add esi, ecx")


        ' Copia i caratteri desiderati
        _LibGen.Append("    rep movsb")
        _LibGen.Append($"    ret")
    End Sub

    Private Sub GenerateStringLeft()
        _LibGen.Append($"StringLeft:")
        ' Copia ecx byte da esi a edi
        _LibGen.Append("    rep movsb")
        _LibGen.Append($"ret")
    End Sub

    Private Sub GenerateStringMid()
        _LibGen.Append($"StringMid:")
        ' ecx contiene l'indice di inizio
        ' edx contiene la lunghezza desiderata
        _LibGen.Append("    dec ecx")
        _LibGen.Append("    add esi, ecx")
        _LibGen.Append("    mov ecx, edx")
        _LibGen.Append("    rep movsb")
        _LibGen.Append($"    ret")
    End Sub

    Private Sub GenerateStringCopy()
        _LibGen.Append($"StringCopy:")
        ' ecx contiene la lunghezza
        _LibGen.Append("    rep movsb")
        _LibGen.Append($"    ret")
    End Sub

    Private Sub GenerateStringConcat()
        _LibGen.Append($"StringConcat:")
        ' Trova la fine della stringa di destinazione
        _LibGen.Append("    mov ecx, 0xFFFFFFFF")
        _LibGen.Append("    xor eax, eax")
        _LibGen.Append("    mov al, 0")
        _LibGen.Append("    repne scasb")
        _LibGen.Append("    dec edi")

        ' Salva i registri utilizzati
        _LibGen.Append("    push esi")
        _LibGen.Append("    push edi")

        ' Calcola la lunghezza della stringa di origine
        _LibGen.Append($"    mov edi, esi")
        _LibGen.Append("    call StringLength")

        ' Ripristina esi e edi
        _LibGen.Append("    pop edi")
        _LibGen.Append("    pop esi")

        ' Copia la stringa sorgente
        _LibGen.Append("    rep movsb")
        _LibGen.Append($"    ret")
    End Sub

    Private Sub GenerateStringLength()
        _LibGen.Append($"StringLength:")
        _LibGen.Append("    mov ecx, 0xFFFFFFFF")
        _LibGen.Append("    xor eax, eax")
        _LibGen.Append("    mov al, 0")
        _LibGen.Append("    repne scasb")
        _LibGen.Append("    not ecx")
        _LibGen.Append("    dec ecx")
        _LibGen.Append($"    ret")
    End Sub

    Private Sub GenerateStringCompare()
        _LibGen.Append($"StringCompare:")

        ' Salva i registri utilizzati
        _LibGen.Append("    push esi")
        _LibGen.Append("    push edi")

        ' Calcola la lunghezza della stringa di origine
        _LibGen.Append("    call StringLength")

        ' Ripristina esi e edi
        _LibGen.Append("    pop edi")
        _LibGen.Append("    pop esi")

        _LibGen.Append("    repe cmpsb")
        _LibGen.Append("    setz al")
        _LibGen.Append("    movzx eax, al")
        _LibGen.Append($"    ret")
    End Sub

End Class