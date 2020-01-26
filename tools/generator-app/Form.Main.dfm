object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'FormMain'
  ClientHeight = 498
  ClientWidth = 824
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object grbxAppCommands: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 818
    Height = 49
    Align = alTop
    Caption = 'Generator Commands'
    TabOrder = 0
    object Button1: TButton
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 180
      Height = 26
      Action = actSelectConnectionDef
      Align = alLeft
      DropDownMenu = pmnRecentConnections
      Style = bsSplitButton
      TabOrder = 0
    end
    object Button2: TButton
      AlignWithMargins = True
      Left = 191
      Top = 18
      Width = 98
      Height = 26
      Action = actConnect
      Align = alLeft
      TabOrder = 1
    end
  end
  object PageControl1: TPageControl
    AlignWithMargins = True
    Left = 152
    Top = 58
    Width = 669
    Height = 437
    ActivePage = tshDataSet
    Align = alRight
    TabOrder = 1
    object tshDataSet: TTabSheet
      Caption = 'Prepare DataDet'
      object Splitter1: TSplitter
        Left = 0
        Top = 225
        Width = 661
        Height = 6
        Cursor = crVSplit
        Align = alTop
        ExplicitTop = 198
        ExplicitWidth = 514
      end
      object GridPanel2: TGridPanel
        Left = 0
        Top = 377
        Width = 661
        Height = 32
        Align = alBottom
        BevelOuter = bvNone
        Caption = ' '
        ColumnCollection = <
          item
            Value = 33.333333333333340000
          end
          item
            Value = 33.333333333333340000
          end
          item
            Value = 33.333333333333340000
          end>
        ControlCollection = <
          item
            Column = 1
            Control = btnGenerateDAO
            Row = 0
          end
          item
            Column = 0
            Control = Label1
            Row = 0
          end>
        RowCollection = <
          item
            Value = 100.000000000000000000
          end>
        TabOrder = 2
        object btnGenerateDAO: TButton
          AlignWithMargins = True
          Left = 223
          Top = 3
          Width = 214
          Height = 26
          Action = actGenerateProxy
          Align = alClient
          TabOrder = 0
        end
        object Label1: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 214
          Height = 26
          Align = alClient
          Alignment = taCenter
          Caption = 'Ctrl+Enter to generate'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGrayText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsItalic]
          ParentFont = False
          WordWrap = True
          ExplicitWidth = 111
          ExplicitHeight = 13
        end
      end
      object DBGrid1: TDBGrid
        AlignWithMargins = True
        Left = 3
        Top = 232
        Width = 655
        Height = 142
        Margins.Top = 1
        Align = alClient
        DataSource = DataSource1
        Options = [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgConfirmDelete, dgCancelOnExit, dgTitleClick, dgTitleHotTrack]
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 661
        Height = 225
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Panel1'
        TabOrder = 0
        object Label2: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 655
          Height = 13
          Margins.Bottom = 0
          Align = alTop
          Caption = 'SQL statement:'
          ExplicitWidth = 75
        end
        object mmSqlStatement: TMemo
          AlignWithMargins = True
          Left = 3
          Top = 19
          Width = 655
          Height = 171
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Consolas'
          Font.Style = []
          Lines.Strings = (
            'mmSqlStatement')
          ParentFont = False
          ScrollBars = ssVertical
          TabOrder = 0
        end
        object GridPanel1: TGridPanel
          Left = 0
          Top = 193
          Width = 661
          Height = 32
          Align = alBottom
          BevelOuter = bvNone
          Caption = ' '
          ColumnCollection = <
            item
              Value = 33.333333333333340000
            end
            item
              Value = 33.333333333333340000
            end
            item
              Value = 33.333333333333340000
            end>
          ControlCollection = <
            item
              Column = 0
              Control = Button3
              Row = 0
            end
            item
              Column = 1
              Control = Label3
              Row = 0
            end
            item
              Column = 2
              Control = Button4
              Row = 0
            end>
          RowCollection = <
            item
              Value = 100.000000000000000000
            end>
          TabOrder = 1
          object Button3: TButton
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 214
            Height = 26
            Action = actExecSQL
            Align = alClient
            TabOrder = 0
          end
          object Label3: TLabel
            AlignWithMargins = True
            Left = 230
            Top = 3
            Width = 200
            Height = 26
            Margins.Left = 10
            Margins.Right = 10
            Align = alClient
            Alignment = taCenter
            Caption = 'Execute SQL to see results in the Data Grid'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGrayText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsItalic]
            ParentFont = False
            WordWrap = True
            ExplicitWidth = 188
          end
          object Button4: TButton
            AlignWithMargins = True
            Left = 443
            Top = 3
            Width = 215
            Height = 26
            Action = actQueryBuilder
            Align = alClient
            TabOrder = 1
          end
        end
      end
    end
    object tshProxyCode: TTabSheet
      Caption = 'Generated code: DataSetProxy'
      ImageIndex = 1
      object mmProxyCode: TMemo
        AlignWithMargins = True
        Left = 199
        Top = 3
        Width = 459
        Height = 403
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Consolas'
        Font.Style = []
        Lines.Strings = (
          'mmProxyCode')
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object grbxProxyGenOptions: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 190
        Height = 403
        Align = alLeft
        Caption = 'Proxy generator options:'
        TabOrder = 1
        object Label4: TLabel
          AlignWithMargins = True
          Left = 7
          Top = 21
          Width = 176
          Height = 13
          Margins.Left = 5
          Margins.Top = 6
          Margins.Right = 5
          Align = alTop
          Caption = 'Proxy name:'
          ExplicitWidth = 61
        end
        object Label5: TLabel
          AlignWithMargins = True
          Left = 5
          Top = 63
          Width = 180
          Height = 13
          Margins.Top = 0
          Margins.Bottom = 8
          Align = alTop
          Caption = 'type [Enter] to apply changes'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clInactiveCaption
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsItalic]
          ParentFont = False
          ExplicitWidth = 144
        end
        object edtProxyName: TEdit
          AlignWithMargins = True
          Left = 7
          Top = 40
          Width = 176
          Height = 21
          Margins.Left = 5
          Margins.Right = 5
          Margins.Bottom = 2
          Align = alTop
          TabOrder = 0
          Text = 'edtProxyName'
          OnKeyPress = edtProxyNameKeyPress
        end
        object GroupBox5: TGroupBox
          AlignWithMargins = True
          Left = 7
          Top = 97
          Width = 176
          Height = 72
          Margins.Left = 5
          Margins.Top = 13
          Margins.Right = 5
          Align = alTop
          Caption = 'Field naming convetion:'
          TabOrder = 1
          object rbtnProxyOptionFieldLowerCase: TRadioButton
            AlignWithMargins = True
            Left = 15
            Top = 18
            Width = 156
            Height = 17
            Margins.Left = 13
            Align = alTop
            Caption = 'Lower case first letter'
            TabOrder = 0
            OnClick = rbtnProxyOptionFieldLowerCaseClick
          end
          object rbtnProxyOptionFieldUpperCase: TRadioButton
            AlignWithMargins = True
            Left = 15
            Top = 41
            Width = 156
            Height = 17
            Margins.Left = 13
            Align = alTop
            Caption = 'Upper case first letter'
            Checked = True
            TabOrder = 1
            TabStop = True
            OnClick = rbtnProxyOptionFieldUpperCaseClick
          end
        end
        object GroupBox6: TGroupBox
          AlignWithMargins = True
          Left = 7
          Top = 185
          Width = 176
          Height = 72
          Margins.Left = 5
          Margins.Top = 13
          Margins.Right = 5
          Align = alTop
          Caption = 'Internal DataSet access:'
          TabOrder = 2
          object rbtnProxyOptionNoDataSetAccess: TRadioButton
            AlignWithMargins = True
            Left = 15
            Top = 18
            Width = 156
            Height = 17
            Margins.Left = 13
            Align = alTop
            Caption = 'No avaliable (sugessted)'
            Checked = True
            TabOrder = 0
            TabStop = True
            OnClick = rbtnProxyOptionNoDataSetAccessClick
          end
          object rbtnProxyOptionCommnetedDataSet: TRadioButton
            AlignWithMargins = True
            Left = 15
            Top = 41
            Width = 156
            Height = 17
            Margins.Left = 13
            Align = alTop
            Caption = 'Commneted property'
            TabOrder = 1
            OnClick = rbtnProxyOptionCommnetedDataSetClick
          end
        end
        object GroupBox7: TGroupBox
          AlignWithMargins = True
          Left = 7
          Top = 273
          Width = 176
          Height = 56
          Margins.Left = 5
          Margins.Top = 13
          Margins.Right = 5
          Align = alTop
          Caption = 'Code identation'
          TabOrder = 3
          object cbxProxyOptionIdentation: TComboBox
            AlignWithMargins = True
            Left = 7
            Top = 18
            Width = 162
            Height = 21
            Margins.Left = 5
            Margins.Right = 5
            Align = alTop
            Style = csDropDownList
            ItemIndex = 0
            TabOrder = 0
            Text = '2 spaces'
            OnChange = cbxProxyOptionIdentationChange
            Items.Strings = (
              '2 spaces'
              '4 spaces')
          end
        end
      end
    end
    object tshFakeDataset: TTabSheet
      Caption = 'Generated code: Fake DataSet'
      ImageIndex = 2
      object gxbxFakeGenOptions: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 190
        Height = 403
        Align = alLeft
        Caption = 'Fake DataSet generator options:'
        TabOrder = 0
        object GroupBox3: TGroupBox
          AlignWithMargins = True
          Left = 7
          Top = 28
          Width = 176
          Height = 69
          Margins.Left = 5
          Margins.Top = 13
          Margins.Right = 5
          Align = alTop
          Caption = 'Generated DataSet:'
          TabOrder = 0
          object rbtnFakeOptionClientDataSet: TRadioButton
            Tag = 12
            AlignWithMargins = True
            Left = 15
            Top = 41
            Width = 156
            Height = 17
            Margins.Left = 13
            Align = alTop
            Caption = 'TClientDataSet'
            TabOrder = 0
            OnClick = rbtnFakeOptionClientDataSetClick
          end
          object rbtnFakeOptionFDMemTable: TRadioButton
            Tag = 11
            AlignWithMargins = True
            Left = 15
            Top = 18
            Width = 156
            Height = 17
            Margins.Left = 13
            Align = alTop
            Caption = 'TFDMemTable'
            Checked = True
            TabOrder = 1
            TabStop = True
            OnClick = rbtnFakeOptionFDMemTableClick
          end
        end
        object GroupBox4: TGroupBox
          AlignWithMargins = True
          Left = 7
          Top = 113
          Width = 176
          Height = 69
          Margins.Left = 5
          Margins.Top = 13
          Margins.Right = 5
          Align = alTop
          Caption = 'Generated Append code:'
          TabOrder = 1
          object rbtnFakeOptionAppendMultiline: TRadioButton
            Tag = 21
            AlignWithMargins = True
            Left = 15
            Top = 18
            Width = 156
            Height = 17
            Margins.Left = 13
            Align = alTop
            Caption = 'Multi line Appends'
            Checked = True
            TabOrder = 0
            TabStop = True
            OnClick = rbtnFakeOptionAppendMultilineClick
          end
          object rbtnFakeOptionAppendSingleline: TRadioButton
            Tag = 22
            AlignWithMargins = True
            Left = 15
            Top = 41
            Width = 156
            Height = 17
            Margins.Left = 13
            Align = alTop
            Caption = 'Single line AppendRecord'
            TabOrder = 1
            OnClick = rbtnFakeOptionAppendSinglelineClick
          end
        end
      end
      object mmFakeDataSetCode: TMemo
        AlignWithMargins = True
        Left = 199
        Top = 3
        Width = 459
        Height = 403
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Consolas'
        Font.Style = []
        Lines.Strings = (
          'mmFakeDataSetCode')
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 1
      end
    end
  end
  object tmrReady: TTimer
    Interval = 1
    OnTimer = tmrReadyTimer
    Left = 48
    Top = 72
  end
  object ActionList1: TActionList
    Left = 48
    Top = 128
    object actConnect: TAction
      Caption = 'Connect'
      OnExecute = actConnectExecute
    end
    object actSelectConnectionDef: TAction
      Caption = 'Select Connection'
      OnExecute = actSelectConnectionDefExecute
    end
    object actExecSQL: TAction
      Caption = 'Exec SQL'
      OnExecute = actExecSQLExecute
    end
    object actGenerateProxy: TAction
      Caption = 'Generate Data Proxy'
      ShortCut = 16397
      OnExecute = actGenerateProxyExecute
    end
    object actQueryBuilder: TAction
      Caption = 'Build SELECT'
      OnExecute = actQueryBuilderExecute
    end
  end
  object DataSource1: TDataSource
    Left = 376
    Top = 320
  end
  object pmnRecentConnections: TPopupMenu
    Left = 48
    Top = 184
  end
end
