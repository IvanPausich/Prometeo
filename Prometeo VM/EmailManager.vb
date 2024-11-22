Imports System.Net.Mail
Imports System.Net
Imports System.Text

Public Class EmailManager
    Private smtpClient As SmtpClient
    Private fromAddress As MailAddress

    Public Sub New(smtpServer As String, port As Integer, username As String, password As String, senderEmail As String)
        smtpClient = New SmtpClient(smtpServer)
        smtpClient.Port = port
        smtpClient.Credentials = New NetworkCredential(username, password)
        smtpClient.EnableSsl = True

        fromAddress = New MailAddress(senderEmail)
    End Sub

    Public Sub SendEmail(toEmail As String, subject As String, body As String, Optional attachmentPath As String = Nothing)
        Try
            Dim message As New MailMessage(fromAddress, New MailAddress(toEmail))
            message.Subject = subject
            message.Body = body
            message.BodyEncoding = Encoding.UTF8
            message.IsBodyHtml = False ' Cambia a True se il corpo è in formato HTML

            If Not String.IsNullOrEmpty(attachmentPath) Then
                Dim attachment As New Attachment(attachmentPath)
                message.Attachments.Add(attachment)
            End If

            smtpClient.Send(message)
            Console.WriteLine("Email inviata con successo.")
        Catch ex As Exception
            Console.WriteLine($"Errore nell'invio dell'email: {ex.Message}")
        End Try
    End Sub

    Public Sub Dispose()
        smtpClient.Dispose()
    End Sub
End Class