Public Module BitManipulation
    Public Codebyte() As Byte
    Public DataByte() As Byte
    ' Funzione per impostare un bit specifico
    Public Function SetBit(value As Byte, bitPosition As Integer) As Byte
        If bitPosition < 0 Or bitPosition > 7 Then
            Throw New ArgumentException("La posizione del bit deve essere tra 0 e 7")
        End If
        Return CByte(value Or (1 << bitPosition))
    End Function

    ' Funzione per cancellare un bit specifico
    Public Function ClearBit(value As Byte, bitPosition As Integer) As Byte
        If bitPosition < 0 Or bitPosition > 7 Then
            Throw New ArgumentException("La posizione del bit deve essere tra 0 e 7")
        End If
        Return CByte(value And Not (1 << bitPosition))
    End Function

    ' Funzione per controllare se un bit specifico è impostato
    Public Function IsBitSet(value As Byte, bitPosition As Integer) As Boolean
        If bitPosition < 0 Or bitPosition > 7 Then
            Throw New ArgumentException("La posizione del bit deve essere tra 0 e 7")
        End If
        Return (value And (1 << bitPosition)) <> 0
    End Function

    ' Funzione per invertire un bit specifico
    Public Function ToggleBit(value As Byte, bitPosition As Integer) As Byte
        If bitPosition < 0 Or bitPosition > 7 Then
            Throw New ArgumentException("La posizione del bit deve essere tra 0 e 7")
        End If
        Return CByte(value Xor (1 << bitPosition))
    End Function

    Public Function EncodeModRM(_mod As Byte, reg As Byte, rm As Byte) As Byte
        ' Assicuriamoci che i valori siano nel range corretto
        If _mod > 3 OrElse reg > 7 OrElse rm > 7 Then
            Throw New ArgumentException("Valori non validi per Mod, Reg o R/M")
        End If

        ' Combiniamo i campi in un singolo byte
        ' Mod occupa i 2 bit più significativi
        ' Reg occupa i 3 bit centrali
        ' R/M occupa i 3 bit meno significativi
        Return CByte((_mod << 6) Or (reg << 3) Or rm)
    End Function

    Public Function EncodeSIB(scale As Byte, index As Byte, base As Byte) As Byte
        ' Assicuriamoci che i valori siano nel range corretto
        If scale > 3 OrElse index > 7 OrElse base > 7 Then
            Throw New ArgumentException("Valori non validi per Scale, Index o Base")
        End If
        ' Combiniamo i campi in un singolo byte
        ' Scale occupa i 2 bit più significativi
        ' Index occupa i 3 bit centrali
        ' Base occupa i 3 bit meno significativi
        Return CByte((scale << 6) Or (index << 3) Or base)
    End Function

    Public Sub Initialize()
        ReDim Codebyte(-1)
        ReDim DataByte(-1)
    End Sub

    ' Add a byte to the array
    Public Sub AddCodeByte(value As Byte)
        ReDim Preserve Codebyte(UBound(Codebyte) + 1)
        Codebyte(UBound(Codebyte)) = CByte(value)
    End Sub

    ' Add a word (2 bytes) to the array
    Public Sub AddCodeWord(value As Object)
        Dim wordBytes() As Byte = BitConverter.GetBytes(value)
        ReDim Preserve Codebyte(UBound(Codebyte) + 2)
        Array.Copy(wordBytes, 0, Codebyte, UBound(Codebyte) - 1, 2)
    End Sub

    ' Add a dword (4 bytes) to the array
    Public Sub AddCodeDWord(value As Object)
        Dim dwordBytes() As Byte = BitConverter.GetBytes(value)
        ReDim Preserve Codebyte(UBound(Codebyte) + 4)
        Array.Copy(dwordBytes, 0, Codebyte, UBound(Codebyte) - 3, 4)
    End Sub

    ' Add a byte to the array
    Public Sub AddDataByte(value As Byte)
        ReDim Preserve Codebyte(UBound(Codebyte) + 1)
        Codebyte(UBound(Codebyte)) = value
    End Sub

    ' Add a word (2 bytes) to the array
    Public Sub AddDataWord(value As Object)
        Dim wordBytes() As Byte = BitConverter.GetBytes(value)
        ReDim Preserve Codebyte(UBound(Codebyte) + 2)
        Array.Copy(wordBytes, 0, Codebyte, UBound(Codebyte) - 1, 2)
    End Sub

    ' Add a dword (4 bytes) to the array
    Public Sub AddDataDWord(value As Object)
        Dim dwordBytes() As Byte = BitConverter.GetBytes(value)
        ReDim Preserve Codebyte(UBound(Codebyte) + 4)
        Array.Copy(dwordBytes, 0, Codebyte, UBound(Codebyte) - 3, 4)
    End Sub

    Public Function ModifyMiddleBits(originalByte As Byte, newMiddleBits As Byte) As Byte
        ' Maschera per isolare i 3 bit centrali
        Const MIDDLE_MASK As Byte = &H1C  ' 00011100 in binario

        ' Maschera per mantenere tutti i bit tranne i 3 centrali
        Const OUTER_MASK As Byte = Not MIDDLE_MASK  ' 11100011 in binario

        ' Manteniamo tutti i bit tranne i 3 centrali
        Dim clearedMiddle As Byte = originalByte And OUTER_MASK

        ' Prepariamo i nuovi 3 bit centrali
        Dim newMiddle As Byte = (newMiddleBits And &H7) << 2

        ' Combiniamo il risultato
        Return clearedMiddle Or newMiddle
    End Function

    Public Function Insert3Bits(originalByte As Byte, newBits As Byte, position As Integer) As Byte
        ' Verifichiamo che la posizione sia valida (0-5)
        If position < 0 Or position > 5 Then
            Throw New ArgumentException("La posizione deve essere tra 0 e 5")
        End If

        ' Maschera per isolare i 3 bit da inserire
        Const NEW_BITS_MASK As Byte = &H7  ' 00000111 in binario

        ' Creiamo una maschera per i 3 bit nella posizione desiderata
        Dim positionMask As Byte = NEW_BITS_MASK << position

        ' Invertiamo la maschera per mantenere tutti gli altri bit
        Dim keepMask As Byte = Not positionMask

        ' Manteniamo tutti i bit tranne quelli nella posizione desiderata
        Dim clearedByte As Byte = originalByte And keepMask

        ' Prepariamo i nuovi 3 bit nella posizione corretta
        Dim shiftedNewBits As Byte = (newBits And NEW_BITS_MASK) << position

        ' Combiniamo il risultato
        Return clearedByte Or shiftedNewBits
    End Function

    Public Function Insert4Bits(originalByte As Byte, newBits As Byte, position As Integer) As Byte
        ' Verifichiamo che la posizione sia valida (0-5)
        If position < 0 Or position > 4 Then
            Throw New ArgumentException("La posizione deve essere tra 0 e 5")
        End If

        ' Maschera per isolare i 3 bit da inserire
        Const NEW_BITS_MASK As Byte = &HF  ' 00001111 in binario

        ' Creiamo una maschera per i 3 bit nella posizione desiderata
        Dim positionMask As Byte = NEW_BITS_MASK << position

        ' Invertiamo la maschera per mantenere tutti gli altri bit
        Dim keepMask As Byte = Not positionMask

        ' Manteniamo tutti i bit tranne quelli nella posizione desiderata
        Dim clearedByte As Byte = originalByte And keepMask

        ' Prepariamo i nuovi 3 bit nella posizione corretta
        Dim shiftedNewBits As Byte = (newBits And NEW_BITS_MASK) << position

        ' Combiniamo il risultato
        Return clearedByte Or shiftedNewBits
    End Function

    Public Function GetSecureImmediate(value As Integer) As Object
        If value >= Byte.MinValue AndAlso value <= Byte.MaxValue Then
            Return CByte(value)
        ElseIf value >= SByte.MinValue AndAlso value <= SByte.MaxValue Then
            Return CSByte(value)
        ElseIf value >= UShort.MinValue AndAlso value <= UShort.MaxValue Then
            Return CUInt(value)
        ElseIf value >= Short.MinValue AndAlso value <= Short.MaxValue Then
            Return CInt(value)
        ElseIf value >= UInteger.MinValue AndAlso value <= UInteger.MaxValue Then
            Return CUInt(value)
        ElseIf value >= Integer.MinValue AndAlso value <= Integer.MaxValue Then
            Return CInt(value)
        Else
            Return value ' Long
        End If
    End Function

    ''' <summary>
    ''' Stampa un array di byte in formato esadecimale a console.
    ''' </summary>
    ''' <param name="bytes">L'array di byte da stampare</param>
    ''' <param name="bytesPerLine">Numero di byte da stampare per riga (default 16)</param>
    Public Sub PrintByteArrayHex(bytes As Byte(), Optional bytesPerLine As Integer = 16)
        If bytes Is Nothing OrElse bytes.Length = 0 Then
            Console.WriteLine("Array vuoto o nullo")
            Return
        End If

        For i As Integer = 0 To bytes.Length - 1
            ' Stampa il valore esadecimale del byte
            Console.Write($"{bytes(i):X2} ")

            ' Vai a capo dopo bytesPerLine byte o all'ultimo byte
            If (i + 1) Mod bytesPerLine = 0 OrElse i = bytes.Length - 1 Then
                Console.WriteLine()
            End If
        Next
    End Sub

    ''' <summary>
    ''' Converte un array di byte in una stringa esadecimale.
    ''' </summary>
    ''' <param name="bytes">L'array di byte da convertire</param>
    ''' <returns>Una stringa rappresentante l'array in formato esadecimale</returns>
    Public Function ByteArrayToHexString(bytes As Byte()) As String
        If bytes Is Nothing OrElse bytes.Length = 0 Then
            Return String.Empty
        End If

        Return BitConverter.ToString(bytes).Replace("-", " ")
    End Function
End Module