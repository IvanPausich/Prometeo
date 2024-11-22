Imports System.IO

Public Class VirtualHardDisk
    Private _offset As UInteger
    Private Const min_SIZE As UInteger = 1024 * 1024 * 50  ' 256 MB
    Private Const minCACHE_SIZE As UInteger = 1024 * 1024  ' 1 MB cache
    Private Const Max_SIZE As UInteger = 1024 * 1024 * 2047  ' 2 GB
    Private Const MaxCACHE_SIZE As UInteger = 1024 * 1024 * 10 ' 256 MB cache
    Private fileStream As FileStream
    Private cacheBuffer() As Byte
    Private cacheStart As UInteger = 0

    Private HD_size As UInteger

    Private CACHE_SIZE As UInteger

    Public Property Pos_Offset() As UInteger
        Get
            Return _offset
        End Get
        Set(valore As UInteger)
            _offset = valore
        End Set
    End Property

    Public Sub New(filePath As String, Optional HD_size As UInteger = min_SIZE, Optional Cache As UInteger = minCACHE_SIZE)
        fileStream = New FileStream(filePath, FileMode.OpenOrCreate, FileAccess.ReadWrite)
        HD_size = IIf(HD_size > Max_SIZE, Max_SIZE, HD_size)
        CACHE_SIZE = IIf(Cache > MaxCACHE_SIZE, MaxCACHE_SIZE, Cache)
        fileStream.SetLength(HD_size)
        cacheBuffer = New Byte(Cache - 1) {}
        LoadCache(0)
    End Sub

    ' New method to change HD size
    Public Sub SetHDSize(newSize As UInteger)
        If newSize < min_SIZE Then
            newSize = min_SIZE
        ElseIf newSize > Max_SIZE Then
            newSize = Max_SIZE
        End If

        HD_size = newSize
        fileStream.SetLength(newSize)
        FlushCache()  ' Ensure any cached data is written before resizing
        If cacheStart >= newSize Then
            cacheStart = 0
            LoadCache(0)
        End If
    End Sub

    ' Nuovo metodo per cancellare la memoria
    Public Sub ClearHD()
        FlushCache()
        fileStream.SetLength(0)
        fileStream.SetLength(HD_size)
        Array.Clear(cacheBuffer, 0, cacheBuffer.Length)
        cacheStart = 0
    End Sub

    Private Function CalculateLinearAddress(offset As UInteger) As UInteger
        Dim linearAddress As UInteger = offset
        If offset > HD_size Then
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

    Public Function ReadByte() As Byte
        Dim linearAddress As UInteger = CalculateLinearAddress(_offset)
        EnsureInCache(linearAddress, 1)
        Return cacheBuffer(CInt(linearAddress - cacheStart))
    End Function

    Public Sub WriteByte(value As Byte)
        Dim linearAddress As UInteger = CalculateLinearAddress(_offset)
        EnsureInCache(linearAddress, 1)
        cacheBuffer(CInt(linearAddress - cacheStart)) = value
    End Sub

    Public Function ReadWord() As UShort
        Dim linearAddress As UInteger = CalculateLinearAddress(_offset)
        EnsureInCache(linearAddress, 2)
        Return BitConverter.ToUInt16(cacheBuffer, CInt(linearAddress - cacheStart))
    End Function

    Public Sub WriteWord(value As UShort)
        Dim linearAddress As UInteger = CalculateLinearAddress(_offset)
        EnsureInCache(linearAddress, 2)
        BitConverter.GetBytes(value).CopyTo(cacheBuffer, CInt(linearAddress - cacheStart))
    End Sub

    Public Function ReadDWord() As UInteger
        Dim linearAddress As UInteger = CalculateLinearAddress(_offset)
        EnsureInCache(linearAddress, 4)
        Return BitConverter.ToUInt32(cacheBuffer, CInt(linearAddress - cacheStart))
    End Function

    Public Sub WriteDWord(value As UInteger)
        Dim linearAddress As UInteger = CalculateLinearAddress(_offset)
        EnsureInCache(linearAddress, 4)
        BitConverter.GetBytes(value).CopyTo(cacheBuffer, CInt(linearAddress - cacheStart))
    End Sub

    Public Sub LoadByteArray(ByRef data As Byte())
        If _offset + data.Length > HD_size Then
            logerror("Tentativo di leggere oltre i limiti della memoria")
        End If

        Dim remainingBytes As Integer = data.Length
        Dim dataOffset As Integer = 0

        While remainingBytes > 0
            Dim bytesToRead As Integer = Math.Min(remainingBytes, CACHE_SIZE)
            Dim HDOffset As UInteger = _offset + CUInt(dataOffset)

            EnsureInCache(HDOffset, bytesToRead)

            Dim cacheOffset As Integer = CInt(HDOffset - cacheStart)
            Array.Copy(cacheBuffer, cacheOffset, data, dataOffset, bytesToRead)

            remainingBytes -= bytesToRead
            dataOffset += bytesToRead
        End While
    End Sub

    Public Sub WriteByteArray(data As Byte())
        If _offset + data.Length > HD_size Then
            logerror("Tentativo di scrivere oltre i limiti della memoria")
        End If

        Dim remainingBytes As Integer = data.Length
        Dim dataOffset As Integer = 0

        While remainingBytes > 0
            Dim bytesToWrite As Integer = Math.Min(remainingBytes, CACHE_SIZE)
            Dim HDOffset As UInteger = _offset + CUInt(dataOffset)

            EnsureInCache(HDOffset, bytesToWrite)

            Dim cacheOffset As Integer = CInt(HDOffset - cacheStart)
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