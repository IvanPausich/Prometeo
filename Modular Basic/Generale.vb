
Module Generale

    Public tokens As List(Of VBParser.Parsed_Code)
    Public currentIndex As Integer = 0
    Public codeStructure As New Stack(Of String)
    Public _CodeGen As New List(Of String)
    Public _LibGen As New List(Of String)
    Public _MemoryManager As New MemoryManager
    Public _SymbolTable As New SymbolTable
    Public _TypeHandler As New VBTypesHandler()

    Public Sub ErrMessage(err As String)

    End Sub
    ' La funzione NextTokenIs esistente può rimanere per compatibilità
    Public Function NextTokenIs(expected As String) As Boolean
        Return TokenAtIs(1, expected)
    End Function

    Private Function TokenAt(offset As Integer) As String
        Dim targetIndex = currentIndex + offset
        If targetIndex >= 0 AndAlso targetIndex < tokens.Count Then
            Return tokens(targetIndex).Token
        Else
            Return String.Empty
        End If
    End Function

    Public Function TokenAtIs(offset As Integer, expected As String) As Boolean
        Return TokenAt(offset).Equals(expected, StringComparison.OrdinalIgnoreCase)
    End Function

    Public Class VBTypesHandler
        ' Costanti per i tipi
        Private Const TYPE_UBYTE As Integer = 0
        Private Const TYPE_BYTE As Integer = 1
        Private Const TYPE_USHORT As Integer = 2
        Private Const TYPE_SHORT As Integer = 3
        Private Const TYPE_UINTEGER As Integer = 4
        Private Const TYPE_INTEGER As Integer = 5
        Private Const TYPE_SINGLE As Integer = 6
        Private Const TYPE_STRING As Integer = 7
        Private Const TYPE_OBJECT As Integer = 9


        Public Function GetTypeCode(typeName As String) As Integer
            Select Case typeName.ToUpper()
                Case "UBYTE"
                    Return TYPE_UBYTE
                Case "BYTE"
                    Return TYPE_INTEGER
                Case "USHORT"
                    Return TYPE_INTEGER
                Case "SHORT"
                    Return TYPE_INTEGER
                Case "UINTEGER"
                    Return TYPE_INTEGER
                Case "INTEGER"
                    Return TYPE_INTEGER
                Case "SINGLE"
                    Return TYPE_SINGLE
                Case "STRING"
                    Return TYPE_STRING
                Case "OBJECT"
                    Return TYPE_OBJECT
                Case Else
                    Return TYPE_OBJECT
            End Select
        End Function

        Public Function GetTypeSize(typeName As String) As Integer
            Select Case typeName.ToUpper()
                Case "INTEGER", "UINTEGER"
                    Return 4
                Case "SINGLE"
                    Return 4
                Case "STRING"
                    Return 4  ' Puntatore a stringa
                Case "BYTE", "UBYTE"
                    Return 1
                Case "OBJECT"
                    Return 4  ' Puntatore a oggetto
                Case "SHORT", "USHORT"
                    Return 2
                Case Else
                    Return 4  ' Puntatore a oggetto
            End Select
        End Function

        Public Function IsNumericType(typeName As String) As Boolean
            Select Case GetTypeCode(typeName)
                Case TYPE_INTEGER, TYPE_UINTEGER, TYPE_SINGLE, TYPE_SHORT, TYPE_USHORT, TYPE_BYTE, TYPE_UBYTE
                    Return True
                Case Else
                    Return False
            End Select
        End Function

        Public Function GetDefaultValue(typeName As String) As String
            Select Case typeName.ToUpper()
                Case "INTEGER", "UINTEGER", "UBYTE", "BYTE", "SHORT", "USHORT"
                    Return "0"
                Case "SINGLE"
                    Return "0.0"
                Case "STRING"
                    Return """"""  ' Stringa vuota
                Case "OBJECT"
                    Return "0"     ' Null pointer
                Case Else
                    Return "0"     ' Null pointer
            End Select
        End Function

        Private Function GetMaxValue(typeName As String) As String
            Select Case typeName.ToUpper()
                Case "INTEGER"
                    Return "2147483647"
                Case "UINTEGER"
                    Return "4294967295"
                Case "SINGLE"
                    Return "3.402823E+38"
                Case "BYTE"
                    Return "127"
                Case "UBYTE"
                    Return "255"
                Case "SHORT"
                    Return "32767"
                Case "USHORT"
                    Return "65535"
                Case Else
                    Return "4294967295"
            End Select
        End Function

        Private Function GetMinValue(typeName As String) As String
            Select Case typeName.ToUpper()
                Case "INTEGER"
                    Return "-214783648"
                Case "UINTEGER"
                    Return "0"
                Case "SINGLE"
                    Return "-3.402823E+38"
                Case "BYTE"
                    Return "-128"
                Case "UBYTE"
                    Return "0"
                Case "SHORT"
                    Return "-32768"
                Case "USHORT"
                    Return "0"
                Case Else
                    Return "0"
            End Select
        End Function

        Public Function ISInRange(TypeName As String, Value As String) As Boolean
            Dim numValue As Double

            ' Prima verifichiamo che Value sia un numero valido
            If Not Double.TryParse(Value, numValue) Then
                Return False
            End If

            ' Poi confrontiamo con min e max del tipo
            Dim minValue As Double
            Dim maxValue As Double

            If Not Double.TryParse(GetMinValue(TypeName), minValue) Then
                Return False
            End If

            If Not Double.TryParse(GetMaxValue(TypeName), maxValue) Then
                Return False
            End If

            Return numValue >= minValue AndAlso numValue <= maxValue
        End Function
    End Class


    ''' Classe che gestisce la tabella dei simboli per tenere traccia di variabili e loro caratteristiche
    Public Class SymbolTable
        ''' Classe interna che rappresenta un singolo simbolo/variabile
        Public Class Symbol
            Public Name As String          ' Nome della variabile
            Public Type As String          ' Tipo della variabile (INTEGER, STRING, etc.)
            Public IsArray As Boolean      ' Se è un array
            Public ArraySize As Integer    ' Dimensione se array, 0 altrimenti
            Public Address As String       ' Indirizzo in memoria dove è allocata
            Public ScopeLevel As Integer   ' Livello di annidamento (0=globale, 1=primo livello, etc.)
            Public IsTemp As Boolean       ' Se è una variabile temporanea per calcoli intermedi
        End Class

        ' Dictionary che mappa nomi variabili ai loro simboli
        Private _symbols As New Dictionary(Of String, Symbol)
        ' Livello di scope corrente (incrementa con ogni Begin/Sub/For/etc.)
        Private _currentScope As Integer = 0

        ''' Costruttore base
        Public Sub New()
        End Sub

        ''' Incrementa il livello di scope quando entriamo in un nuovo blocco
        Public Sub EnterScope()
            _currentScope += 1
        End Sub

        ''' Dealloca variabili dello scope corrente quando usciamo dal blocco
        Public Sub ExitScope()
            ' Trova tutte le variabili del livello corrente
            Dim toRemove = _symbols.Where(Function(s) s.Value.ScopeLevel = _currentScope).ToList()

            ' Dealloca ogni variabile trovata
            For Each symbol In toRemove
                _MemoryManager.DeallocateVariable(symbol.Value.Name, symbol.Value.Type,
                                                If(symbol.Value.IsArray, symbol.Value.ArraySize, 0))
                _symbols.Remove(symbol.Key)
            Next

            ' Torna al livello di scope precedente
            _currentScope -= 1
        End Sub

        ''' Restituisci tutte le variabili dello scope corrente quando usciamo dal blocco
        Public Function VarScope() As List(Of String)
            ' Trova tutte le variabili del livello corrente
            Dim toRemove = _symbols.Where(Function(s) s.Value.ScopeLevel = _currentScope).ToList()
            Dim result As New List(Of String)
            ' Dealloca ogni variabile trovata
            For Each symbol In toRemove
                result.Append(symbol.Value.Name)
            Next
            Return result
        End Function


        ''' Dichiara una nuova variabile e la registra nella tabella
        ''' name: nome variabile
        ''' type: tipo variabile
        ''' arraySize: se > 0, è un array di questa dimensione
        ''' isTemp: se è una variabile temporanea
        Public Function DeclareVariable(name As String, type As String,
                                      Optional arraySize As Integer = 0,
                                      Optional isTemp As Boolean = False) As String
            If Not ExistSymbol(name) Then
                ' Alloca memoria fisica per la variabile
                _MemoryManager.DeclareVariable(name, type, arraySize)

                ' Registra la variabile nella tabella dei simboli
                _symbols(name) = New Symbol With {
                    .Name = name,
                    .Type = type,
                    .IsArray = arraySize > 0,
                    .ArraySize = arraySize,
                    .ScopeLevel = _currentScope,
                    .IsTemp = isTemp
                }
                Return name
            End If
            Return ""
        End Function

        ''' Crea una variabile temporanea per risultati intermedi
        Public Function CreateTempVariable(type As String) As String
            ' Nome univoco basato sul numero di simboli
            Dim tempName = $"_temp_{_symbols.Count}"
            Return DeclareVariable(tempName, type, 0, True)
        End Function

        ''' Rimuove un simbolo e dealloca la memoria associata
        Public Sub RemoveSymbol(name As String)
            If _symbols.ContainsKey(name) Then
                Dim symbol = _symbols(name)
                ' Dealloca la memoria
                _MemoryManager.DeallocateVariable(name, symbol.Type,
                                                If(symbol.IsArray, symbol.ArraySize, 0))
                ' Rimuove dalla tabella
                _symbols.Remove(name)
            End If
        End Sub

        ''' Ottiene il tipo di una variabile
        Public Function GetSymbolType(name As String) As String
            If _symbols.ContainsKey(name) Then
                Return _symbols(name).Type
            End If
            Return "OBJECT"  ' Tipo di default
        End Function

        ''' Verifica se una variabile è un array
        Public Function IsArray(name As String) As Boolean
            Return _symbols.ContainsKey(name) AndAlso _symbols(name).IsArray
        End Function

        ''' Ottiene la dimensione dell'array
        Public Function GetArraySize(name As String) As Integer
            If _symbols.ContainsKey(name) AndAlso _symbols(name).IsArray Then
                Return _symbols(name).ArraySize
            End If
            Return 0  ' Non è un array o non esiste
        End Function

        ''' Verifica se esiste una variabile
        Public Function SymbolExists(name As String) As Boolean
            Return _symbols.ContainsKey(name)
        End Function

        ''' Ottiene tutte le informazioni di un simbolo
        Public Function GetSymbol(name As String) As Symbol
            Return If(_symbols.ContainsKey(name), _symbols(name), Nothing)
        End Function

        ''' Ottiene tutte le informazioni di un simbolo
        Public Function ExistSymbol(name As String) As Boolean
            Return If(_symbols.ContainsKey(name), True, False)
        End Function

    End Class
End Module
