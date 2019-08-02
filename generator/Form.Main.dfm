object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'FormMain'
  ClientHeight = 437
  ClientWidth = 669
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
    Width = 663
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
    Left = 144
    Top = 58
    Width = 522
    Height = 376
    ActivePage = tshProxyCode
    Align = alRight
    TabOrder = 1
    object tshDataSet: TTabSheet
      Caption = 'SQL Statement'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Splitter1: TSplitter
        Left = 0
        Top = 225
        Width = 514
        Height = 6
        Cursor = crVSplit
        Align = alTop
        ExplicitTop = 198
      end
      object GridPanel2: TGridPanel
        Left = 0
        Top = 316
        Width = 514
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
          Left = 174
          Top = 3
          Width = 165
          Height = 26
          Action = actGenerateProxy
          Align = alClient
          TabOrder = 0
        end
        object Label1: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 111
          Height = 13
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
        end
      end
      object DBGrid1: TDBGrid
        AlignWithMargins = True
        Left = 3
        Top = 232
        Width = 508
        Height = 81
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
        Width = 514
        Height = 225
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Panel1'
        TabOrder = 0
        object Label2: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 508
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
          Width = 508
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
          Width = 514
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
            Width = 165
            Height = 26
            Action = actExecSQL
            Align = alClient
            TabOrder = 0
          end
          object Label3: TLabel
            AlignWithMargins = True
            Left = 181
            Top = 3
            Width = 143
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
          end
          object Button4: TButton
            AlignWithMargins = True
            Left = 345
            Top = 3
            Width = 166
            Height = 26
            Action = actQueryBuilder
            Align = alClient
            TabOrder = 1
          end
        end
      end
    end
    object tshProxyCode: TTabSheet
      Caption = 'Generated DAO Code'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object mmProxyCode: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 508
        Height = 158
        Align = alTop
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
