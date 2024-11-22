' Classe per gestire le stringhe
Public Class StringHandler

    ' Costanti per i codici di errore
    Private Const ERROR_BUFFER_OVERFLOW As Integer = 1
    Private Const ERROR_INVALID_INDEX As Integer = 2
    Private Const ERROR_NULL_STRING As Integer = 3

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
        _CodeGen.AppendLine("    ; Controllo parametri")
        _CodeGen.AppendLine($"    cmp {src}, 0")
        _CodeGen.AppendLine("    je null_string_error")
        _CodeGen.AppendLine($"    cmp {dest}, 0")
        _CodeGen.AppendLine("    je null_string_error")

        Select Case operation
            Case "COPY"
                ' Passa i parametri e chiama la funzione per copiare una stringa
                _CodeGen.AppendLine($"    mov esi, {src}")
                _CodeGen.AppendLine($"    mov edi, {dest}")
                If length IsNot Nothing Then
                    _CodeGen.AppendLine($"    mov ecx, {length}")
                Else
                    ' Calcola lunghezze
                    _CodeGen.AppendLine("    push esi")              ' Salva puntatori
                    _CodeGen.AppendLine("    push edi")
                    _CodeGen.AppendLine($"    mov edi, [{src}]")
                    _CodeGen.AppendLine("    call StringLength")
                    _CodeGen.AppendLine("    pop edi")
                    _CodeGen.AppendLine("    pop esi")              ' Salva puntatori
                End If
                _CodeGen.AppendLine($"    call StringCopy")
            Case "&"
                ' Passa i parametri e chiama la funzione per concatenare due stringhe
                _CodeGen.AppendLine($"    mov esi, {src}")
                _CodeGen.AppendLine($"    mov edi, {dest}")
                _CodeGen.AppendLine($"    call StringConcat")
            Case "LEN"
                ' Passa i parametri e chiama la funzione per ottenere la lunghezza di una stringa
                _CodeGen.AppendLine($"    mov edi, {src}")
                _CodeGen.AppendLine($"    call StringLength")
                _CodeGen.AppendLine($"    mov [{dest}], ecx")
            Case "COMPARE"
                ' Passa i parametri e chiama la funzione per confrontare due stringhe
                _CodeGen.AppendLine($"    mov esi, {dest}")
                _CodeGen.AppendLine($"    mov edi, {src}")
                _CodeGen.AppendLine($"    call StringCompare")
                _CodeGen.AppendLine($"    mov [{dest}], eax")
            Case "UCASE"
                ' Passa i parametri e chiama la funzione per convertire una stringa in maiuscolo
                _CodeGen.AppendLine($"    mov esi, {src}")
                _CodeGen.AppendLine($"    mov edi, {dest}")
                _CodeGen.AppendLine($"    call StringUCase")
            Case "LCASE"
                ' Passa i parametri e chiama la funzione per convertire una stringa in minuscolo
                _CodeGen.AppendLine($"    mov esi, {src}")
                _CodeGen.AppendLine($"    mov edi, {dest}")
                _CodeGen.AppendLine($"    call StringLCase")
            Case "RIGHT"
                ' Passa i parametri e chiama la funzione per ottenere i caratteri a destra di una stringa
                _CodeGen.AppendLine($"    mov esi, {src}")
                _CodeGen.AppendLine($"    mov edi, {dest}")
                _CodeGen.AppendLine($"    mov ecx, {length}")
                _CodeGen.AppendLine($"    call StringRight")
            Case "LEFT"
                ' Passa i parametri e chiama la funzione per ottenere i caratteri a sinistra di una stringa
                _CodeGen.AppendLine($"    mov esi, {src}")
                _CodeGen.AppendLine($"    mov edi, {dest}")
                _CodeGen.AppendLine($"    mov ecx, {length}")
                _CodeGen.AppendLine($"    call StringLeft")
            Case "MID"
                ' Passa i parametri e chiama la funzione per ottenere i caratteri centrali di una stringa
                _CodeGen.AppendLine($"    mov esi, {src}")
                _CodeGen.AppendLine($"    mov edi, {dest}")
                _CodeGen.AppendLine($"    mov ecx, {start}")
                _CodeGen.AppendLine($"    mov edx, {length}")
                _CodeGen.AppendLine($"    call StringMid")
            Case "TRIM"
                _CodeGen.AppendLine($"    mov esi, {src}")
                _CodeGen.AppendLine($"    mov edi, {dest}")
                _CodeGen.AppendLine("    call StringTrim")

            Case "REPLACE"
                _CodeGen.AppendLine($"    mov esi, {src}")           ' Stringa originale
                _CodeGen.AppendLine($"    mov edi, {dest}")          ' Destinazione
                _CodeGen.AppendLine($"    mov edx, {start}")         ' Stringa da cercare
                _CodeGen.AppendLine($"    mov ebx, {replacement}")   ' Stringa sostitutiva
                _CodeGen.AppendLine("    call StringReplace")

            Case "INDEXOF"
                _CodeGen.AppendLine($"    mov esi, {src}")          ' Stringa in cui cercare
                _CodeGen.AppendLine($"    mov edi, {start}")        ' Sottostringa da trovare
                _CodeGen.AppendLine($"    mov [{dest}], eax")       ' Salva risultato
                _CodeGen.AppendLine("    call StringIndexOf")
        End Select

    End Sub

    Private Sub GenerateStringTrim()
        _LibGen.AppendLine("StringTrim:")
        ' Trova inizio (salta spazi iniziali)
        _LibGen.AppendLine("    push esi")              ' Salva puntatore originale
        _LibGen.AppendLine(".skip_spaces_start:")
        _LibGen.AppendLine("    lodsb")
        _LibGen.AppendLine("    cmp al, ' '")
        _LibGen.AppendLine("    je .skip_spaces_start")
        _LibGen.AppendLine("    cmp al, 9")            ' Tab
        _LibGen.AppendLine("    je .skip_spaces_start")
        _LibGen.AppendLine("    dec esi")              ' Torna all'ultimo carattere non spazio

        ' Trova fine (salta spazi finali)
        _LibGen.AppendLine("    mov edx, esi")         ' Salva inizio stringa pulita
        _LibGen.AppendLine("    call StringLength")
        _LibGen.AppendLine("    add esi, ecx")         ' Vai alla fine
        _LibGen.AppendLine("    dec esi")

        _LibGen.AppendLine(".skip_spaces_end:")
        _LibGen.AppendLine("    mov al, [esi]")
        _LibGen.AppendLine("    cmp al, ' '")
        _LibGen.AppendLine("    je .next_end")
        _LibGen.AppendLine("    cmp al, 9")
        _LibGen.AppendLine("    jne .copy_trim")

        _LibGen.AppendLine(".next_end:")
        _LibGen.AppendLine("    dec esi")
        _LibGen.AppendLine("    jmp .skip_spaces_end")

        ' Copia la stringa pulita
        _LibGen.AppendLine(".copy_trim:")
        _LibGen.AppendLine("    mov ecx, esi")
        _LibGen.AppendLine("    sub ecx, edx")
        _LibGen.AppendLine("    inc ecx")              ' Include l'ultimo carattere
        _LibGen.AppendLine("    mov esi, edx")
        _LibGen.AppendLine("    rep movsb")
        _LibGen.AppendLine("    mov byte [edi], 0")    ' Termina la stringa
        _LibGen.AppendLine("    pop esi")              ' Ripristina puntatore
        _LibGen.AppendLine("    ret")
    End Sub

    Private Sub GenerateStringReplace()
        _LibGen.AppendLine("StringReplace:")
        ' esi = stringa originale
        ' edi = destinazione
        ' edx = stringa da cercare
        ' ebx = stringa sostitutiva

        _LibGen.AppendLine("    push esi")              ' Salva puntatori
        _LibGen.AppendLine("    push edi")

        ' Calcola lunghezze
        _LibGen.AppendLine("    push edx")
        _LibGen.AppendLine("    mov edi, edx")
        _LibGen.AppendLine("    call StringLength")
        _LibGen.AppendLine("    mov edx, ecx")          ' edx = lunghezza da cercare
        _LibGen.AppendLine("    pop edi")

        _LibGen.AppendLine(".search_loop:")
        _LibGen.AppendLine("    mov ecx, edx")          ' Lunghezza da confrontare
        _LibGen.AppendLine("    push esi")
        _LibGen.AppendLine("    push edi")
        _LibGen.AppendLine("    repe cmpsb")            ' Confronta
        _LibGen.AppendLine("    pop edi")
        _LibGen.AppendLine("    pop esi")

        _LibGen.AppendLine("    je .replace_found")     ' Se trovato, sostituisci

        _LibGen.AppendLine("    lodsb")                 ' Altrimenti copia carattere
        _LibGen.AppendLine("    stosb")
        _LibGen.AppendLine("    test al, al")
        _LibGen.AppendLine("    jnz .search_loop")
        _LibGen.AppendLine("    jmp .replace_done")

        _LibGen.AppendLine(".replace_found:")
        _LibGen.AppendLine("    push esi")
        _LibGen.AppendLine("    mov esi, ebx")          ' Copia stringa sostitutiva
        _LibGen.AppendLine("    call StringLength")
        _LibGen.AppendLine("    rep movsb")
        _LibGen.AppendLine("    pop esi")
        _LibGen.AppendLine("    add esi, edx")          ' Salta stringa originale
        _LibGen.AppendLine("    jmp .search_loop")

        _LibGen.AppendLine(".replace_done:")
        _LibGen.AppendLine("    pop edi")
        _LibGen.AppendLine("    pop esi")
        _LibGen.AppendLine("    ret")
    End Sub

    Private Sub GenerateStringIndexOf()
        _LibGen.AppendLine("StringIndexOf:")
        ' esi = stringa in cui cercare
        ' edi = sottostringa da trovare

        _LibGen.AppendLine("    push esi")
        _LibGen.AppendLine("    push edi")

        ' Calcola lunghezza sottostringa
        _LibGen.AppendLine("    push esi")
        _LibGen.AppendLine("    mov esi, edi")
        _LibGen.AppendLine("    call StringLength")
        _LibGen.AppendLine("    mov edx, ecx")         ' edx = lunghezza da cercare
        _LibGen.AppendLine("    pop esi")

        _LibGen.AppendLine("    xor ebx, ebx")         ' ebx = posizione corrente

        _LibGen.AppendLine(".search_loop:")
        _LibGen.AppendLine("    mov ecx, edx")         ' Lunghezza da confrontare
        _LibGen.AppendLine("    push esi")
        _LibGen.AppendLine("    push edi")
        _LibGen.AppendLine("    repe cmpsb")           ' Confronta
        _LibGen.AppendLine("    pop edi")
        _LibGen.AppendLine("    pop esi")

        _LibGen.AppendLine("    je .found")            ' Se trovato

        _LibGen.AppendLine("    inc ebx")
        _LibGen.AppendLine("    inc esi")
        _LibGen.AppendLine("    cmp byte [esi], 0")
        _LibGen.AppendLine("    jne .search_loop")

        _LibGen.AppendLine("    mov ebx, -1")          ' Non trovato

        _LibGen.AppendLine(".found:")
        _LibGen.AppendLine("    mov eax, ebx")         ' Ritorna posizione
        _LibGen.AppendLine("    pop edi")
        _LibGen.AppendLine("    pop esi")
        _LibGen.AppendLine("    ret")
    End Sub

    Private Sub GenerateStringUCase()
        _LibGen.AppendLine($"Ucase:")
        ' Imposta ecx a 0xFFFFFFFF per scorrere l'intera stringa
        _LibGen.AppendLine("    mov ecx, 0xFFFFFFFF")
        ' Etichetta per l'inizio del ciclo
        _LibGen.AppendLine(".loop:")
        ' Carica un byte da esi in al e incrementa esi
        _LibGen.AppendLine("    lodsb")
        ' Confronta al con 0 (fine stringa)
        _LibGen.AppendLine("    cmp al, 0")
        ' Se al è 0, salta a .done
        _LibGen.AppendLine("    je .done")
        ' Confronta al con 'a'
        _LibGen.AppendLine("    cmp al, 'a'")
        ' Se al è minore di 'a', salta a .skip
        _LibGen.AppendLine("    jb .skip")
        ' Confronta al con 'z'
        _LibGen.AppendLine("    cmp al, 'z'")
        ' Se al è maggiore di 'z', salta a .skip
        _LibGen.AppendLine("    ja .skip")
        ' Sottrae 32 da al per convertire in maiuscolo
        _LibGen.AppendLine("    sub al, 32")
        ' Etichetta .skip
        _LibGen.AppendLine(".skip:")
        ' Memorizza al in edi e incrementa edi
        _LibGen.AppendLine("    stosb")
        ' Salta a .loop per continuare il ciclo
        _LibGen.AppendLine("    jmp .loop")
        ' Etichetta .done per la fine del ciclo
        _LibGen.AppendLine(".done:")
        _LibGen.AppendLine($"ret")
    End Sub

    Private Sub GenerateStringLCase()
        _LibGen.AppendLine($"Lcase:")
        ' Imposta ecx a 0xFFFFFFFF per scorrere l'intera stringa
        _LibGen.AppendLine("    mov ecx, 0xFFFFFFFF")
        ' Etichetta per l'inizio del ciclo
        _LibGen.AppendLine(".loop:")
        ' Carica un byte da esi in al e incrementa esi
        _LibGen.AppendLine("    lodsb")
        ' Confronta al con 0 (fine stringa)
        _LibGen.AppendLine("    cmp al, 0")
        ' Se al è 0, salta a .done
        _LibGen.AppendLine("    je .done")
        ' Confronta al con 'A'
        _LibGen.AppendLine("    cmp al, 'A'")
        ' Se al è minore di 'A', salta a .skip
        _LibGen.AppendLine("    jb .skip")
        ' Confronta al con 'Z'
        _LibGen.AppendLine("    cmp al, 'Z'")
        ' Se al è maggiore di 'Z', salta a .skip
        _LibGen.AppendLine("    ja .skip")
        ' Aggiunge 32 ad al per convertire in minuscolo
        _LibGen.AppendLine("    add al, 32")
        ' Etichetta .skip
        _LibGen.AppendLine(".skip:")
        ' Memorizza al in edi e incrementa edi
        _LibGen.AppendLine("    stosb")
        ' Salta a .loop per continuare il ciclo
        _LibGen.AppendLine("    jmp .loop")
        ' Etichetta .done per la fine del ciclo
        _LibGen.AppendLine(".done:")
        _LibGen.AppendLine($"ret")
    End Sub

    Private Sub GenerateStringRight()
        _LibGen.AppendLine($"StringRight:")
        ' sposta in eax il numero di caratteri da restituire
        _LibGen.AppendLine("    mov eax, ecx")

        ' Salva i registri utilizzati
        _LibGen.AppendLine("    push esi")
        _LibGen.AppendLine("    push edi")

        ' Calcola la lunghezza della stringa di origine
        _LibGen.AppendLine($"    mov edi, esi")
        _LibGen.AppendLine("    call StringLength")

        ' Ripristina esi e edi
        _LibGen.AppendLine("    pop edi")
        _LibGen.AppendLine("    pop esi")

        ' Calcola la posizione di inizio
        _LibGen.AppendLine("    sub ecx, eax")
        _LibGen.AppendLine("    add esi, ecx")


        ' Copia i caratteri desiderati
        _LibGen.AppendLine("    rep movsb")
        _LibGen.AppendLine($"    ret")
    End Sub

    Private Sub GenerateStringLeft()
        _LibGen.AppendLine($"StringLeft:")
        ' Copia ecx byte da esi a edi
        _LibGen.AppendLine("    rep movsb")
        _LibGen.AppendLine($"ret")
    End Sub

    Private Sub GenerateStringMid()
        _LibGen.AppendLine($"StringMid:")
        ' ecx contiene l'indice di inizio
        ' edx contiene la lunghezza desiderata
        _LibGen.AppendLine("    dec ecx")
        _LibGen.AppendLine("    add esi, ecx")
        _LibGen.AppendLine("    mov ecx, edx")
        _LibGen.AppendLine("    rep movsb")
        _LibGen.AppendLine($"    ret")
    End Sub

    Private Sub GenerateStringCopy()
        _LibGen.AppendLine($"StringCopy:")
        ' ecx contiene la lunghezza
        _LibGen.AppendLine("    rep movsb")
        _LibGen.AppendLine($"    ret")
    End Sub

    Private Sub GenerateStringConcat()
        _LibGen.AppendLine($"StringConcat:")
        ' Trova la fine della stringa di destinazione
        _LibGen.AppendLine("    mov ecx, 0xFFFFFFFF")
        _LibGen.AppendLine("    xor eax, eax")
        _LibGen.AppendLine("    mov al, 0")
        _LibGen.AppendLine("    repne scasb")
        _LibGen.AppendLine("    dec edi")

        ' Salva i registri utilizzati
        _LibGen.AppendLine("    push esi")
        _LibGen.AppendLine("    push edi")

        ' Calcola la lunghezza della stringa di origine
        _LibGen.AppendLine($"    mov edi, esi")
        _LibGen.AppendLine("    call StringLength")

        ' Ripristina esi e edi
        _LibGen.AppendLine("    pop edi")
        _LibGen.AppendLine("    pop esi")

        ' Copia la stringa sorgente
        _LibGen.AppendLine("    rep movsb")
        _LibGen.AppendLine($"    ret")
    End Sub

    Private Sub GenerateStringLength()
        _LibGen.AppendLine($"StringLength:")
        _LibGen.AppendLine("    mov ecx, 0xFFFFFFFF")
        _LibGen.AppendLine("    xor eax, eax")
        _LibGen.AppendLine("    mov al, 0")
        _LibGen.AppendLine("    repne scasb")
        _LibGen.AppendLine("    not ecx")
        _LibGen.AppendLine("    dec ecx")
        _LibGen.AppendLine($"    ret")
    End Sub

    Private Sub GenerateStringCompare()
        _LibGen.AppendLine($"StringCompare:")

        ' Salva i registri utilizzati
        _LibGen.AppendLine("    push esi")
        _LibGen.AppendLine("    push edi")

        ' Calcola la lunghezza della stringa di origine
        _LibGen.AppendLine("    call StringLength")

        ' Ripristina esi e edi
        _LibGen.AppendLine("    pop edi")
        _LibGen.AppendLine("    pop esi")

        _LibGen.AppendLine("    repe cmpsb")
        _LibGen.AppendLine("    setz al")
        _LibGen.AppendLine("    movzx eax, al")
        _LibGen.AppendLine($"    ret")
    End Sub

End Class