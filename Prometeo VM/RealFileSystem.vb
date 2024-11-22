Imports System.IO

Public Class RealFileSystem
    Private Const PORT_OPERATION As UShort = &H400
    Private Const PORT_FILENAME As UShort = &H401
    Private Const PORT_DATA As UShort = &H402
    Private Const PORT_STATUS As UShort = &H403

    Private currentOperation As Byte = 0
    Private currentFilename As String = ""
    Private currentFileStream As FileStream = Nothing
    Private dataBuffer As New List(Of Byte)
    Private statusCode As Byte = 0

    ' Operazioni
    Private Const OP_OPEN_READ As Byte = 1
    Private Const OP_OPEN_WRITE As Byte = 2
    Private Const OP_CLOSE As Byte = 3
    Private Const OP_READ As Byte = 4
    Private Const OP_WRITE As Byte = 5
    Private Const OP_DELETE As Byte = 6

    Public Function ReadPort(port As UShort) As Byte
        Select Case port
            Case PORT_STATUS
                Return statusCode
            Case PORT_DATA
                If dataBuffer.Count > 0 Then
                    Dim value = dataBuffer(0)
                    dataBuffer.RemoveAt(0)
                    Return value
                Else
                    Return 0
                End If
            Case Else
                Return 0
        End Select
    End Function

    Public Sub WritePort(port As UShort, value As Byte)
        Select Case port
            Case PORT_OPERATION
                ExecuteOperation(value)
            Case PORT_FILENAME
                currentFilename += Convert.ToChar(value)
            Case PORT_DATA
                dataBuffer.Add(value)
        End Select
    End Sub

    Private Sub ExecuteOperation(operation As Byte)
        currentOperation = operation
        statusCode = 0

        Select Case operation
            Case OP_OPEN_READ
                Try
                    currentFileStream = New FileStream(currentFilename, FileMode.Open, FileAccess.Read)
                    statusCode = 1 ' Success
                Catch ex As Exception
                    statusCode = 2 ' Error
                End Try
                currentFilename = ""

            Case OP_OPEN_WRITE
                Try
                    currentFileStream = New FileStream(currentFilename, FileMode.Create, FileAccess.Write)
                    statusCode = 1 ' Success
                Catch ex As Exception
                    statusCode = 2 ' Error
                End Try
                currentFilename = ""

            Case OP_CLOSE
                If currentFileStream IsNot Nothing Then
                    currentFileStream.Close()
                    currentFileStream = Nothing
                    statusCode = 1 ' Success
                Else
                    statusCode = 2 ' Error
                End If

            Case OP_READ
                If currentFileStream IsNot Nothing AndAlso currentFileStream.CanRead Then
                    Dim buffer(1023) As Byte
                    Dim bytesRead = currentFileStream.Read(buffer, 0, buffer.Length)
                    dataBuffer.AddRange(buffer.Take(bytesRead))
                    statusCode = 1 ' Success
                Else
                    statusCode = 2 ' Error
                End If

            Case OP_WRITE
                If currentFileStream IsNot Nothing AndAlso currentFileStream.CanWrite Then
                    currentFileStream.Write(dataBuffer.ToArray(), 0, dataBuffer.Count)
                    dataBuffer.Clear()
                    statusCode = 1 ' Success
                Else
                    statusCode = 2 ' Error
                End If

            Case OP_DELETE
                Try
                    File.Delete(currentFilename)
                    statusCode = 1 ' Success
                Catch ex As Exception
                    statusCode = 2 ' Error
                End Try
                currentFilename = ""

        End Select
    End Sub

    Public Sub Dispose()
        If currentFileStream IsNot Nothing Then
            currentFileStream.Close()
            currentFileStream = Nothing
        End If
    End Sub
End Class