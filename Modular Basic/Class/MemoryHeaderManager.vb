Public Class MemoryHeaderManager

    Public Sub New()
        ' _CodeGen = New System.Text.StringBuilder()
        ' _LibGen = New System.Text.StringBuilder()
        GenerateHeaderManagementCode()
    End Sub

    Private Sub GenerateHeaderManagementCode()
        GenerateHeaderConstants()
        GenerateHeaderInit()
        GenerateHeaderAllocate()
        GenerateHeaderDeallocate()
    End Sub

    Private Sub GenerateHeaderConstants()
        _LibGen.Append("    ; Costanti di memoria")
        _LibGen.Append("    HEADER_START:    data 0x1C00000  ; Inizio header")
        _LibGen.Append("    HEADER_END:      data 0x1C0FFFF  ; Fine header")
        _LibGen.Append("    DATA_START:      data 0x1C10000  ; Inizio area dati")
        _LibGen.Append("    MEMORY_END:      data 0xB3FFFFF  ; Fine memoria")
        _LibGen.Append("    CURRENT_POS:     data 0x1C00008  ; Posizione corrente nell'header")
        _LibGen.Append("    BLOCKS_COUNT:    data 0          ; Numero di blocchi")
    End Sub

    Private Sub GenerateHeaderInit()
        _LibGen.Append("HeaderInit:")
        _LibGen.Append("    push ebx")
        _LibGen.Append("    push ecx")

        ' Inizializza header vuoto
        _LibGen.Append("    mov eax, [HEADER_START]")
        _LibGen.Append("    mov dword [eax], 0        ; Blocks count = 0")
        _LibGen.Append("    add eax, 4")
        _LibGen.Append("    mov dword [eax], 0        ; First free = null")

        ' Inizializza primo blocco libero
        _LibGen.Append("    mov eax, [DATA_START]")
        _LibGen.Append("    mov ebx, [MEMORY_END]")
        _LibGen.Append("    sub ebx, eax              ; Calcola spazio totale disponibile")

        ' Crea prima entry
        _LibGen.Append("    mov ecx, [CURRENT_POS]")
        _LibGen.Append("    mov [ecx], ebx            ; Salva dimensione")
        _LibGen.Append("    add ecx, 4")
        _LibGen.Append("    mov [ecx], eax            ; Salva indirizzo")
        _LibGen.Append("    add ecx, 4")
        _LibGen.Append("    mov [CURRENT_POS], ecx")

        ' Incrementa contatore blocchi
        _LibGen.Append("    mov eax, [BLOCKS_COUNT]")
        _LibGen.Append("    inc eax")
        _LibGen.Append("    mov [BLOCKS_COUNT], eax")

        _LibGen.Append("    pop ecx")
        _LibGen.Append("    pop ebx")
        _LibGen.Append("    ret")
    End Sub

    Private Sub GenerateHeaderAllocate()
        _LibGen.Append("HeaderAllocate:")
        _LibGen.Append("    ; Input: ECX = dimensione richiesta")
        _LibGen.Append("    ; Output: EAX = indirizzo allocato o 0 se fallito")
        _LibGen.Append("    push ebx")
        _LibGen.Append("    push esi")
        _LibGen.Append("    push edi")

        ' Inizia ricerca dal primo blocco
        _LibGen.Append("    mov esi, [HEADER_START]")
        _LibGen.Append("    add esi, 8               ; Salta header info")

        _LibGen.Append("    mov edx, [BLOCKS_COUNT]")
        _LibGen.Append("    test edx, edx")
        _LibGen.Append("    jz .no_memory            ; Se non ci sono blocchi, errore")

        ' Loop attraverso i blocchi
        _LibGen.Append(".find_block:")
        _LibGen.Append("    mov eax, [esi]           ; Carica dimensione")
        _LibGen.Append("    cmp eax, ecx             ; Confronta con richiesta")
        _LibGen.Append("    jae .block_found         ; Se abbastanza grande, usa questo")

        _LibGen.Append("    add esi, 8               ; Passa al prossimo blocco")
        _LibGen.Append("    cmp esi, [CURRENT_POS]   ; Verifica se fine header")
        _LibGen.Append("    jae .no_memory")

        _LibGen.Append("    jmp .find_block")

        ' Blocco trovato
        _LibGen.Append(".block_found:")
        _LibGen.Append("    mov ebx, [esi + 4]       ; Carica indirizzo")
        _LibGen.Append("    sub eax, ecx             ; Calcola spazio rimanente")
        _LibGen.Append("    jz .exact_match          ; Se esatto, rimuovi blocco")

        ' Aggiorna blocco esistente
        _LibGen.Append("    mov [esi], eax           ; Aggiorna dimensione")
        _LibGen.Append("    add dword [esi + 4], ecx ; Aggiorna indirizzo")
        _LibGen.Append("    mov eax, ebx             ; Ritorna indirizzo allocato")
        _LibGen.Append("    jmp .done")

        ' Rimuovi blocco (match esatto)
        _LibGen.Append(".exact_match:")
        _LibGen.Append("    push esi")
        _LibGen.Append("    call RemoveBlock")
        _LibGen.Append("    pop esi")
        _LibGen.Append("    mov eax, ebx             ; Ritorna indirizzo allocato")
        _LibGen.Append("    jmp .done")

        ' Memoria esaurita
        _LibGen.Append(".no_memory:")
        _LibGen.Append("    xor eax, eax")

        _LibGen.Append(".done:")
        _LibGen.Append("    pop edi")
        _LibGen.Append("    pop esi")
        _LibGen.Append("    pop ebx")
        _LibGen.Append("    ret")
    End Sub

    Private Sub GenerateHeaderDeallocate()
        _LibGen.Append("HeaderDeallocate:")
        _LibGen.Append("    ; Input: EAX = indirizzo da deallocare, ECX = dimensione")
        _LibGen.Append("    push ebx")
        _LibGen.Append("    push esi")
        _LibGen.Append("    push edi")

        ' Verifica overflow header
        _LibGen.Append("    mov esi, [CURRENT_POS]")
        _LibGen.Append("    add esi, 8               ; Spazio per nuova entry")
        _LibGen.Append("    cmp esi, [HEADER_END]")
        _LibGen.Append("    ja .error               ; Se overflow, errore")

        ' Inserisci nuovo blocco libero
        _LibGen.Append("    mov esi, [CURRENT_POS]")
        _LibGen.Append("    mov [esi], ecx           ; Salva dimensione")
        _LibGen.Append("    mov [esi + 4], eax       ; Salva indirizzo")
        _LibGen.Append("    add esi, 8")
        _LibGen.Append("    mov [CURRENT_POS], esi")

        ' Incrementa contatore blocchi
        _LibGen.Append("    inc dword [BLOCKS_COUNT]")

        ' Prova a unire blocchi adiacenti
        _LibGen.Append("    call MergeBlocks")

        _LibGen.Append(".error:")
        _LibGen.Append("    pop edi")
        _LibGen.Append("    pop esi")
        _LibGen.Append("    pop ebx")
        _LibGen.Append("    ret")

        ' Funzione di unione blocchi adiacenti
        _LibGen.Append("MergeBlocks:")
        _LibGen.Append("    push eax")
        _LibGen.Append("    push ebx")
        _LibGen.Append("    push esi")
        _LibGen.Append("    push edi")

        _LibGen.Append("    mov esi, [HEADER_START]")
        _LibGen.Append("    add esi, 8               ; Salta header info")

        _LibGen.Append(".merge_loop:")
        _LibGen.Append("    cmp esi, [CURRENT_POS]")
        _LibGen.Append("    jae .merge_done")

        _LibGen.Append("    mov eax, [esi + 4]       ; Indirizzo blocco corrente")
        _LibGen.Append("    mov ebx, [esi]           ; Dimensione blocco corrente")
        _LibGen.Append("    add ebx, eax             ; Fine blocco corrente")

        _LibGen.Append("    mov edi, esi")
        _LibGen.Append("    add edi, 8")
        _LibGen.Append("    cmp edi, [CURRENT_POS]")
        _LibGen.Append("    jae .next_block")

        _LibGen.Append("    cmp ebx, [edi + 4]       ; Confronta con inizio prossimo")
        _LibGen.Append("    jne .next_block")

        ' Unisci blocchi
        _LibGen.Append("    mov eax, [edi]           ; Dimensione secondo blocco")
        _LibGen.Append("    add [esi], eax           ; Somma dimensioni")
        _LibGen.Append("    push edi")
        _LibGen.Append("    call RemoveBlock")
        _LibGen.Append("    pop edi")
        _LibGen.Append("    jmp .merge_loop")

        _LibGen.Append(".next_block:")
        _LibGen.Append("    add esi, 8")
        _LibGen.Append("    jmp .merge_loop")

        _LibGen.Append(".merge_done:")
        _LibGen.Append("    pop edi")
        _LibGen.Append("    pop esi")
        _LibGen.Append("    pop ebx")
        _LibGen.Append("    pop eax")
        _LibGen.Append("    ret")
    End Sub

End Class
