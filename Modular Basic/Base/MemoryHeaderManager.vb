Public Class MemoryHeaderManager
    ' Costanti per la gestione della memoria
    Private Const MEMORY_START As UInteger = &H1C00000      ' Inizio memoria totale
    Private Const HEADER_START As UInteger = &H1C00000      ' Inizio header
    Private Const HEADER_END As UInteger = &H1C0FFFF        ' Fine header   (64KB per header)
    Private Const DATA_START As UInteger = &H1C10000        ' Inizio dati
    Private Const MEMORY_END As UInteger = &HB3FFFFF        ' Fine memoria


    Public Sub New()
        _CodeGen = New System.Text.StringBuilder()
        _LibGen = New System.Text.StringBuilder()
        GenerateHeaderManagementCode()
    End Sub

    Private Sub GenerateHeaderManagementCode()
        GenerateHeaderConstants()
        GenerateHeaderInit()
        GenerateHeaderAllocate()
        GenerateHeaderDeallocate()
    End Sub

    Private Sub GenerateHeaderConstants()
        _LibGen.AppendLine("    ; Costanti di memoria")
        _LibGen.AppendLine("    HEADER_START:    data 0x1C00000  ; Inizio header")
        _LibGen.AppendLine("    HEADER_END:      data 0x1C0FFFF  ; Fine header")
        _LibGen.AppendLine("    DATA_START:      data 0x1C10000  ; Inizio area dati")
        _LibGen.AppendLine("    MEMORY_END:      data 0xB3FFFFF  ; Fine memoria")
        _LibGen.AppendLine("    CURRENT_POS:     data 0x1C00008  ; Posizione corrente nell'header")
        _LibGen.AppendLine("    BLOCKS_COUNT:    data 0          ; Numero di blocchi")
    End Sub

    Private Sub GenerateHeaderInit()
        _LibGen.AppendLine("HeaderInit:")
        _LibGen.AppendLine("    push ebx")
        _LibGen.AppendLine("    push ecx")

        ' Inizializza header vuoto
        _LibGen.AppendLine("    mov eax, [HEADER_START]")
        _LibGen.AppendLine("    mov dword [eax], 0        ; Blocks count = 0")
        _LibGen.AppendLine("    add eax, 4")
        _LibGen.AppendLine("    mov dword [eax], 0        ; First free = null")

        ' Inizializza primo blocco libero
        _LibGen.AppendLine("    mov eax, [DATA_START]")
        _LibGen.AppendLine("    mov ebx, [MEMORY_END]")
        _LibGen.AppendLine("    sub ebx, eax              ; Calcola spazio totale disponibile")

        ' Crea prima entry
        _LibGen.AppendLine("    mov ecx, [CURRENT_POS]")
        _LibGen.AppendLine("    mov [ecx], ebx            ; Salva dimensione")
        _LibGen.AppendLine("    add ecx, 4")
        _LibGen.AppendLine("    mov [ecx], eax            ; Salva indirizzo")
        _LibGen.AppendLine("    add ecx, 4")
        _LibGen.AppendLine("    mov [CURRENT_POS], ecx")

        ' Incrementa contatore blocchi
        _LibGen.AppendLine("    mov eax, [BLOCKS_COUNT]")
        _LibGen.AppendLine("    inc eax")
        _LibGen.AppendLine("    mov [BLOCKS_COUNT], eax")

        _LibGen.AppendLine("    pop ecx")
        _LibGen.AppendLine("    pop ebx")
        _LibGen.AppendLine("    ret")
    End Sub

    Private Sub GenerateHeaderAllocate()
        _LibGen.AppendLine("HeaderAllocate:")
        _LibGen.AppendLine("    ; Input: ECX = dimensione richiesta")
        _LibGen.AppendLine("    ; Output: EAX = indirizzo allocato o 0 se fallito")
        _LibGen.AppendLine("    push ebx")
        _LibGen.AppendLine("    push esi")
        _LibGen.AppendLine("    push edi")

        ' Inizia ricerca dal primo blocco
        _LibGen.AppendLine("    mov esi, [HEADER_START]")
        _LibGen.AppendLine("    add esi, 8               ; Salta header info")

        _LibGen.AppendLine("    mov edx, [BLOCKS_COUNT]")
        _LibGen.AppendLine("    test edx, edx")
        _LibGen.AppendLine("    jz .no_memory            ; Se non ci sono blocchi, errore")

        ' Loop attraverso i blocchi
        _LibGen.AppendLine(".find_block:")
        _LibGen.AppendLine("    mov eax, [esi]           ; Carica dimensione")
        _LibGen.AppendLine("    cmp eax, ecx             ; Confronta con richiesta")
        _LibGen.AppendLine("    jae .block_found         ; Se abbastanza grande, usa questo")

        _LibGen.AppendLine("    add esi, 8               ; Passa al prossimo blocco")
        _LibGen.AppendLine("    cmp esi, [CURRENT_POS]   ; Verifica se fine header")
        _LibGen.AppendLine("    jae .no_memory")

        _LibGen.AppendLine("    jmp .find_block")

        ' Blocco trovato
        _LibGen.AppendLine(".block_found:")
        _LibGen.AppendLine("    mov ebx, [esi + 4]       ; Carica indirizzo")
        _LibGen.AppendLine("    sub eax, ecx             ; Calcola spazio rimanente")
        _LibGen.AppendLine("    jz .exact_match          ; Se esatto, rimuovi blocco")

        ' Aggiorna blocco esistente
        _LibGen.AppendLine("    mov [esi], eax           ; Aggiorna dimensione")
        _LibGen.AppendLine("    add dword [esi + 4], ecx ; Aggiorna indirizzo")
        _LibGen.AppendLine("    mov eax, ebx             ; Ritorna indirizzo allocato")
        _LibGen.AppendLine("    jmp .done")

        ' Rimuovi blocco (match esatto)
        _LibGen.AppendLine(".exact_match:")
        _LibGen.AppendLine("    push esi")
        _LibGen.AppendLine("    call RemoveBlock")
        _LibGen.AppendLine("    pop esi")
        _LibGen.AppendLine("    mov eax, ebx             ; Ritorna indirizzo allocato")
        _LibGen.AppendLine("    jmp .done")

        ' Memoria esaurita
        _LibGen.AppendLine(".no_memory:")
        _LibGen.AppendLine("    xor eax, eax")

        _LibGen.AppendLine(".done:")
        _LibGen.AppendLine("    pop edi")
        _LibGen.AppendLine("    pop esi")
        _LibGen.AppendLine("    pop ebx")
        _LibGen.AppendLine("    ret")
    End Sub

    Private Sub GenerateHeaderDeallocate()
        _LibGen.AppendLine("HeaderDeallocate:")
        _LibGen.AppendLine("    ; Input: EAX = indirizzo da deallocare, ECX = dimensione")
        _LibGen.AppendLine("    push ebx")
        _LibGen.AppendLine("    push esi")
        _LibGen.AppendLine("    push edi")

        ' Verifica overflow header
        _LibGen.AppendLine("    mov esi, [CURRENT_POS]")
        _LibGen.AppendLine("    add esi, 8               ; Spazio per nuova entry")
        _LibGen.AppendLine("    cmp esi, [HEADER_END]")
        _LibGen.AppendLine("    ja .error               ; Se overflow, errore")

        ' Inserisci nuovo blocco libero
        _LibGen.AppendLine("    mov esi, [CURRENT_POS]")
        _LibGen.AppendLine("    mov [esi], ecx           ; Salva dimensione")
        _LibGen.AppendLine("    mov [esi + 4], eax       ; Salva indirizzo")
        _LibGen.AppendLine("    add esi, 8")
        _LibGen.AppendLine("    mov [CURRENT_POS], esi")

        ' Incrementa contatore blocchi
        _LibGen.AppendLine("    inc dword [BLOCKS_COUNT]")

        ' Prova a unire blocchi adiacenti
        _LibGen.AppendLine("    call MergeBlocks")

        _LibGen.AppendLine(".error:")
        _LibGen.AppendLine("    pop edi")
        _LibGen.AppendLine("    pop esi")
        _LibGen.AppendLine("    pop ebx")
        _LibGen.AppendLine("    ret")

        ' Funzione di unione blocchi adiacenti
        _LibGen.AppendLine("MergeBlocks:")
        _LibGen.AppendLine("    push eax")
        _LibGen.AppendLine("    push ebx")
        _LibGen.AppendLine("    push esi")
        _LibGen.AppendLine("    push edi")

        _LibGen.AppendLine("    mov esi, [HEADER_START]")
        _LibGen.AppendLine("    add esi, 8               ; Salta header info")

        _LibGen.AppendLine(".merge_loop:")
        _LibGen.AppendLine("    cmp esi, [CURRENT_POS]")
        _LibGen.AppendLine("    jae .merge_done")

        _LibGen.AppendLine("    mov eax, [esi + 4]       ; Indirizzo blocco corrente")
        _LibGen.AppendLine("    mov ebx, [esi]           ; Dimensione blocco corrente")
        _LibGen.AppendLine("    add ebx, eax             ; Fine blocco corrente")

        _LibGen.AppendLine("    mov edi, esi")
        _LibGen.AppendLine("    add edi, 8")
        _LibGen.AppendLine("    cmp edi, [CURRENT_POS]")
        _LibGen.AppendLine("    jae .next_block")

        _LibGen.AppendLine("    cmp ebx, [edi + 4]       ; Confronta con inizio prossimo")
        _LibGen.AppendLine("    jne .next_block")

        ' Unisci blocchi
        _LibGen.AppendLine("    mov eax, [edi]           ; Dimensione secondo blocco")
        _LibGen.AppendLine("    add [esi], eax           ; Somma dimensioni")
        _LibGen.AppendLine("    push edi")
        _LibGen.AppendLine("    call RemoveBlock")
        _LibGen.AppendLine("    pop edi")
        _LibGen.AppendLine("    jmp .merge_loop")

        _LibGen.AppendLine(".next_block:")
        _LibGen.AppendLine("    add esi, 8")
        _LibGen.AppendLine("    jmp .merge_loop")

        _LibGen.AppendLine(".merge_done:")
        _LibGen.AppendLine("    pop edi")
        _LibGen.AppendLine("    pop esi")
        _LibGen.AppendLine("    pop ebx")
        _LibGen.AppendLine("    pop eax")
        _LibGen.AppendLine("    ret")
    End Sub

End Class
