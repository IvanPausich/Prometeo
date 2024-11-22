Imports System.Drawing.Imaging
Imports System.Runtime.InteropServices

Public Class VirtualMonitor
    ' Enumerazione per le risoluzioni supportate
    Public Enum Resolution
        ResPersonalizzata = &H0
        Res320x240 = &H1
        Res640x480 = &H2
        Res800x600 = &H3
        Res1024x768 = &H4
        Res1280x720 = &H5
        Res1366x768 = &H6
        Res1600x900 = &H7
        Res1920x1080 = &H8
    End Enum

    ' Dizionario per mappare le risoluzioni alle dimensioni effettive
    Private Shared ReadOnly ResolutionSizes As New Dictionary(Of Resolution, (Width As Integer, Height As Integer)) From {
        {Resolution.ResPersonalizzata, (0, 0)},
        {Resolution.Res320x240, (320, 240)},
        {Resolution.Res640x480, (640, 480)},
        {Resolution.Res800x600, (800, 600)},
        {Resolution.Res1024x768, (1024, 768)},
        {Resolution.Res1280x720, (1280, 720)},
        {Resolution.Res1366x768, (1366, 768)},
        {Resolution.Res1600x900, (1600, 900)},
        {Resolution.Res1920x1080, (1920, 1080)}
    }

    Private VramWidth As UInteger = 320
    Private VramHeight As UInteger = 240
    Private VramSize As UInteger
    Private vram() As Byte
    Private virtualScreen As Bitmap
    Private bitmapData As BitmapData
    Private ptr As IntPtr

    Public Property Width As UInteger
        Get
            Return VramWidth
        End Get
        Set(value As UInteger)
            VramWidth = value
        End Set
    End Property

    Public Property Height As UInteger
        Get
            Return VramHeight
        End Get
        Set(value As UInteger)
            VramHeight = value
        End Set
    End Property

    Public ReadOnly Property Size As UInteger
        Get
            Return VramSize
        End Get
    End Property

    ' Costruttore che accetta un'enumerazione Resolution
    Public Sub New(resolution As Byte)
        ResizeVideo(resolution)
    End Sub

    ' Metodo privato per ridimensionare il video
    Public Sub ResizeVideo(resolution As Byte)
        Dim newSize = ResolutionSizes(resolution)
        If resolution = &H0 Then
            VramSize = CUInt(VramWidth * VramWidth * 3)
        Else
            VramWidth = CUInt(newSize.Width)
            VramWidth = CUInt(newSize.Height)
            VramSize = CUInt(newSize.Width * newSize.Height * 3)
        End If
        ReDim vram(VramSize - 1)
        virtualScreen = New Bitmap(newSize.Width, newSize.Height)
    End Sub

    ' Metodo per disegnare lo schermo virtuale
    Public Sub DrawVirtualScreen()
        bitmapData = virtualScreen.LockBits(New Rectangle(0, 0, virtualScreen.Width, virtualScreen.Height), ImageLockMode.WriteOnly, PixelFormat.Format32bppArgb)
        ptr = bitmapData.Scan0

        Dim stride As Integer = bitmapData.Stride
        Dim vramIndex As Integer = 0

        For y As Integer = 0 To VramHeight - 1
            For x As Integer = 0 To VramWidth - 1
                Dim r As Byte = vram(vramIndex)
                Dim g As Byte = vram(vramIndex + 1)
                Dim b As Byte = vram(vramIndex + 2)
                Dim color As Integer = (CInt(r) << 16) Or (CInt(g) << 8) Or b Or (&HFF << 24)
                Marshal.WriteInt32(ptr, y * stride + x * 4, color)
                vramIndex += 3
            Next
        Next

        virtualScreen.UnlockBits(bitmapData)
    End Sub

    ' Metodo sicuro per scrivere nella VRAM
    Public Sub WriteVram(data() As Byte)
        If data.Length <> VramSize Then
            logerror($"Dimensione dati non valida. Atteso: {VramSize}, Ricevuto: {data.Length}")
        End If
        Array.Copy(data, vram, VramSize)
    End Sub

    Public Sub ClearVram()
        Array.Clear(vram, 0, CInt(VramSize))
    End Sub

    ' Metodo per ottenere l'immagine del monitor virtuale
    Public Function GetVirtualScreen() As Bitmap
        Return virtualScreen
    End Function
End Class