Imports System.IO

Public Class VMem
    Private _MEMORY_SIZE As ULong
    Private _CACHE_SIZE As UInteger

    Private Const min_SIZE As ULong = 1024 * 1024 * 256  ' 256 MB
    Private Const min_CACHE_SIZE As UInteger = 1024 * 1024  ' 1 MB cache
    Private Const Max_SIZE As ULong = 1024 * 1024 * 2047  ' 2 GB
    Private Const Max_CACHE_SIZE As UInteger = 1024 * 1024 * 256 ' 256 MB cache
    Private fileStream As FileStream
    Private cacheBuffer() As Byte
    Private cacheStart As ULong = 0

    Public ReadOnly Property Memory_size() As ULong
        Get
            Return _MEMORY_SIZE
        End Get
    End Property

    Public ReadOnly Property CACHE_SIZE() As UInteger
        Get
            Return _CACHE_SIZE
        End Get
    End Property

    Public Sub New(filePath As String, Optional Mem_size As UInteger = min_SIZE, Optional Cache As UInteger = min_CACHE_SIZE)
        fileStream = New FileStream(filePath, FileMode.OpenOrCreate, FileAccess.ReadWrite)
        _MEMORY_SIZE = IIf(Mem_size > Max_SIZE, Max_SIZE, Mem_size)
        _CACHE_SIZE = IIf(Cache > Max_CACHE_SIZE, Max_CACHE_SIZE, Cache)
        fileStream.SetLength(Mem_size)
        cacheBuffer = New Byte(Cache - 1) {}
        LoadCache(0)
    End Sub

    ' New method to change memory size
    Public Sub SetMemorySize(newSize As ULong)
        If newSize < min_SIZE Then
            newSize = min_SIZE
        ElseIf newSize > Max_SIZE Then
            newSize = Max_SIZE
        End If

        _MEMORY_SIZE = newSize
        fileStream.SetLength(newSize)
        FlushCache()  ' Ensure any cached data is written before resizing
        If cacheStart >= newSize Then
            cacheStart = 0
            LoadCache(0)
        End If
    End Sub

    ' Nuovo metodo per cancellare la memoria
    Public Sub ClearMemory()
        FlushCache()
        fileStream.SetLength(0)
        fileStream.SetLength(_MEMORY_SIZE)
        Array.Clear(cacheBuffer, 0, cacheBuffer.Length)
        cacheStart = 0
    End Sub

    Private Function CalculateLinearAddress(offset As UInteger) As UInteger
        Dim linearAddress As UInteger = offset
        If offset > MEMORY_SIZE Then
            LogError("Violazione del limite")
        End If
        Return linearAddress And &HFFFFFFFFUI
    End Function

    Private Sub EnsureInCache(offset As Long, size As Integer)
        If offset < cacheStart OrElse offset + size > cacheStart + CACHE_SIZE Then
            FlushCache()
            LoadCache(offset)
        End If
    End Sub

    Private Sub LoadCache(offset As Long)
        cacheStart = offset
        fileStream.Seek(offset, SeekOrigin.Begin)
        fileStream.Read(cacheBuffer, 0, CACHE_SIZE)
    End Sub

    Private Sub FlushCache()
        fileStream.Seek(cacheStart, SeekOrigin.Begin)
        fileStream.Write(cacheBuffer, 0, CACHE_SIZE)
    End Sub

    Public Function ReadByte(offset As UInteger) As Byte
        Dim linearAddress As UInteger = CalculateLinearAddress(offset)
        EnsureInCache(linearAddress, 1)
        Return cacheBuffer(CInt(linearAddress - cacheStart))
    End Function

    Public Sub WriteByte(offset As UInteger, value As Byte)
        Dim linearAddress As UInteger = CalculateLinearAddress(offset)
        EnsureInCache(linearAddress, 1)
        cacheBuffer(CInt(linearAddress - cacheStart)) = value
    End Sub

    Public Function ReadWord(offset As UInteger) As UShort
        Dim linearAddress As UInteger = CalculateLinearAddress(offset)
        EnsureInCache(linearAddress, 2)
        Return BitConverter.ToUInt16(cacheBuffer, CInt(linearAddress - cacheStart))
    End Function

    Public Sub WriteWord(offset As UInteger, value As UShort)
        Dim linearAddress As UInteger = CalculateLinearAddress(offset)
        EnsureInCache(linearAddress, 2)
        BitConverter.GetBytes(value).CopyTo(cacheBuffer, CInt(linearAddress - cacheStart))
    End Sub

    Public Function ReadDWord(offset As UInteger) As UInteger
        Dim linearAddress As UInteger = CalculateLinearAddress(offset)
        EnsureInCache(linearAddress, 4)
        Return BitConverter.ToUInt32(cacheBuffer, CInt(linearAddress - cacheStart))
    End Function

    Public Sub WriteDWord(offset As UInteger, value As UInteger)
        Dim linearAddress As UInteger = CalculateLinearAddress(offset)
        EnsureInCache(linearAddress, 4)
        BitConverter.GetBytes(value).CopyTo(cacheBuffer, CInt(linearAddress - cacheStart))
    End Sub

    Public Function ReadQWord(offset As UInteger) As ULong
        Dim linearAddress As UInteger = CalculateLinearAddress(offset)
        EnsureInCache(linearAddress, 8)
        Return BitConverter.ToUInt64(cacheBuffer, CInt(linearAddress - cacheStart))
    End Function

    Public Sub WriteQWord(offset As UInteger, value As ULong)
        Dim linearAddress As UInteger = CalculateLinearAddress(offset)
        EnsureInCache(linearAddress, 8)
        BitConverter.GetBytes(value).CopyTo(cacheBuffer, CInt(linearAddress - cacheStart))
    End Sub

    Public Function ReadDQWord(offset As UInteger) As Byte()
        Dim linearAddress As UInteger = CalculateLinearAddress(offset)
        EnsureInCache(linearAddress, 16)
        Dim result(15) As Byte
        Array.Copy(cacheBuffer, CInt(linearAddress - cacheStart), result, 0, 16)
        Return result
    End Function

    Public Sub WriteDQWord(offset As UInteger, value As Byte())
        If value.Length <> 16 Then
            logerror("DQWord value must be an array of 16 bytes")
        End If
        Dim linearAddress As UInteger = CalculateLinearAddress(offset)
        EnsureInCache(linearAddress, 16)
        Array.Copy(value, 0, cacheBuffer, CInt(linearAddress - cacheStart), 16)
    End Sub

    Public Function Read80Bit(offset As UInteger) As Byte()
        Dim linearAddress As UInteger = CalculateLinearAddress(offset)
        EnsureInCache(linearAddress, 10)
        Dim result(9) As Byte
        Array.Copy(cacheBuffer, CInt(linearAddress - cacheStart), result, 0, 10)
        Return result
    End Function

    Public Sub Write80Bit(offset As UInteger, value As Byte())
        If value.Length <> 10 Then
            logerror("80-bit value must be an array of 10 bytes")
        End If
        Dim linearAddress As UInteger = CalculateLinearAddress(offset)
        EnsureInCache(linearAddress, 10)
        value.CopyTo(cacheBuffer, CInt(linearAddress - cacheStart))
    End Sub

    Public Function ReadSingle(offset As UInteger) As Single
        Dim linearAddress As UInteger = CalculateLinearAddress(offset)
        EnsureInCache(linearAddress, 4)
        Return BitConverter.ToSingle(cacheBuffer, CInt(linearAddress - cacheStart))
    End Function

    Public Sub WriteSingle(offset As UInteger, value As Single)
        Dim linearAddress As UInteger = CalculateLinearAddress(offset)
        EnsureInCache(linearAddress, 4)
        BitConverter.GetBytes(value).CopyTo(cacheBuffer, CInt(linearAddress - cacheStart))
    End Sub

    Public Sub LoadByteArray(ByRef data As Byte(), offset As UInteger)
        If offset + data.Length > _MEMORY_SIZE Then
            logerror("Tentativo di leggere oltre i limiti della memoria")
        End If

        Dim remainingBytes As Integer = data.Length
        Dim dataOffset As Integer = 0

        While remainingBytes > 0
            Dim bytesToRead As Integer = Math.Min(remainingBytes, _CACHE_SIZE)
            Dim memoryOffset As UInteger = offset + CUInt(dataOffset)

            EnsureInCache(memoryOffset, bytesToRead)

            Dim cacheOffset As Integer = CInt(memoryOffset - cacheStart)
            Array.Copy(cacheBuffer, cacheOffset, data, dataOffset, bytesToRead)

            remainingBytes -= bytesToRead
            dataOffset += bytesToRead
        End While
    End Sub

    Public Sub WriteByteArray(data As Byte(), offset As UInteger)
        If offset + data.Length > _MEMORY_SIZE Then
            logerror("Tentativo di scrivere oltre i limiti della memoria")
        End If

        Dim remainingBytes As Integer = data.Length
        Dim dataOffset As Integer = 0

        While remainingBytes > 0
            Dim bytesToWrite As Integer = Math.Min(remainingBytes, _CACHE_SIZE)
            Dim memoryOffset As UInteger = offset + CUInt(dataOffset)

            EnsureInCache(memoryOffset, bytesToWrite)

            Dim cacheOffset As Integer = CInt(memoryOffset - cacheStart)
            Array.Copy(data, dataOffset, cacheBuffer, cacheOffset, bytesToWrite)

            remainingBytes -= bytesToWrite
            dataOffset += bytesToWrite
        End While

        FlushCache()  ' Ensure the written data is flushed to the file
    End Sub

    Public Sub Dispose()
            FlushCache()
            fileStream.Close()
            fileStream.Dispose()
        End Sub
End Class
