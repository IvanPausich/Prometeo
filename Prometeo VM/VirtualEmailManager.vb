Public Class VirtualEmailManager
    Private Const PORT_OPERATION As UShort = &H700
    Private Const PORT_DATA As UShort = &H701
    Private Const PORT_STATUS As UShort = &H702

    Private emailManager As EmailManager
    Private currentOperation As Byte = 0
    Private dataBuffer As New List(Of Byte)
    Private statusCode As Byte = 0

    ' Operazioni
    Private Const OP_SET_RECIPIENT As Byte = 1
    Private Const OP_SET_SUBJECT As Byte = 2
    Private Const OP_SET_BODY As Byte = 3
    Private Const OP_SET_ATTACHMENT As Byte = 4
    Private Const OP_SEND_EMAIL As Byte = 5

    Private recipient As String = ""
    Private subject As String = ""
    Private body As String = ""
    Private attachmentPath As String = ""

    Public Sub New(smtpServer As String, port As Integer, username As String, password As String, senderEmail As String)
        emailManager = New EmailManager(smtpServer, port, username, password, senderEmail)
    End Sub

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
            Case PORT_DATA
                dataBuffer.Add(value)
        End Select
    End Sub

    Private Sub ExecuteOperation(operation As Byte)
        currentOperation = operation
        statusCode = 0

        Select Case operation
            Case OP_SET_RECIPIENT
                recipient = System.Text.Encoding.UTF8.GetString(dataBuffer.ToArray())
                dataBuffer.Clear()
                statusCode = 1 ' Success

            Case OP_SET_SUBJECT
                subject = System.Text.Encoding.UTF8.GetString(dataBuffer.ToArray())
                dataBuffer.Clear()
                statusCode = 1 ' Success

            Case OP_SET_BODY
                body = System.Text.Encoding.UTF8.GetString(dataBuffer.ToArray())
                dataBuffer.Clear()
                statusCode = 1 ' Success

            Case OP_SET_ATTACHMENT
                attachmentPath = System.Text.Encoding.UTF8.GetString(dataBuffer.ToArray())
                dataBuffer.Clear()
                statusCode = 1 ' Success

            Case OP_SEND_EMAIL
                Try
                    emailManager.SendEmail(recipient, subject, body, attachmentPath)
                    statusCode = 1 ' Success
                Catch ex As Exception
                    statusCode = 2 ' Error
                End Try
                ' Reset all fields after sending
                recipient = ""
                subject = ""
                body = ""
                attachmentPath = ""

            Case Else
                statusCode = 2 ' Error
        End Select
    End Sub

    Public Sub Dispose()
        emailManager.Dispose()
    End Sub
End Class