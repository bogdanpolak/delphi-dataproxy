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
    ExplicitWidth = 663
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
    ActivePage = tshProxyCode
    Align = alRight
    TabOrder = 1
    ExplicitHeight = 418
    object tshDataSet: TTabSheet
      Caption = 'SQL Statement'
      ExplicitWidth = 514
      ExplicitHeight = 348
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
        ExplicitTop = 316
        ExplicitWidth = 514
        object btnGenerateDAO: TButton
          AlignWithMargins = True
          Left = 223
          Top = 3
          Width = 214
          Height = 26
          Action = actGenerateProxy
          Align = alClient
          TabOrder = 0
          ExplicitLeft = 174
          ExplicitWidth = 165
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
        ExplicitWidth = 514
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
          ExplicitWidth = 508
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
          ExplicitWidth = 514
          object Button3: TButton
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 214
            Height = 26
            Action = actExecSQL
            Align = alClient
            TabOrder = 0
            ExplicitWidth = 165
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
            ExplicitLeft = 181
            ExplicitWidth = 143
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
            ExplicitLeft = 345
            ExplicitWidth = 166
          end
        end
      end
    end
    object tshProxyCode: TTabSheet
      Caption = 'Generated DAO Code'
      ImageIndex = 1
      ExplicitWidth = 514
      ExplicitHeight = 390
      object mmProxyCode: TMemo
        AlignWithMargins = True
        Left = 183
        Top = 3
        Width = 475
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
        ExplicitLeft = 150
        ExplicitWidth = 361
        ExplicitHeight = 342
      end
      object GroupBox1: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 174
        Height = 403
        Align = alLeft
        Caption = 'Proxy parameters'
        TabOrder = 1
        ExplicitHeight = 342
        object Label4: TLabel
          AlignWithMargins = True
          Left = 7
          Top = 21
          Width = 160
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
          Width = 164
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
          Width = 160
          Height = 21
          Margins.Left = 5
          Margins.Right = 5
          Margins.Bottom = 2
          Align = alTop
          TabOrder = 0
          Text = 'edtProxyName'
          OnKeyPress = edtProxyNameKeyPress
          ExplicitWidth = 127
        end
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
    object actChangeProxyName: TAction
      Caption = 'Change Proxy Name'
      OnExecute = actChangeProxyNameExecute
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
