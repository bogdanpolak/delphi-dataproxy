object DialogQueryBuilder: TDialogQueryBuilder
  Left = 0
  Top = 0
  Caption = 'SQL Query Builder'
  ClientHeight = 424
  ClientWidth = 666
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
  object pnTables: TPanel
    Left = 0
    Top = 0
    Width = 193
    Height = 391
    Align = alLeft
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 0
    ExplicitLeft = 89
    ExplicitTop = 8
    object Label1: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 187
      Height = 13
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Main table / view:'
      ExplicitLeft = 4
      ExplicitTop = 4
      ExplicitWidth = 85
    end
    object Label2: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 46
      Width = 187
      Height = 13
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Join table / view (multi-select):'
      ExplicitLeft = 4
      ExplicitTop = 47
      ExplicitWidth = 147
    end
    object cbxTablesMain: TComboBox
      AlignWithMargins = True
      Left = 3
      Top = 19
      Width = 187
      Height = 21
      Align = alTop
      TabOrder = 0
      Text = 'cbxTablesMain'
      ExplicitLeft = 24
      ExplicitTop = 96
      ExplicitWidth = 145
    end
    object lbxJoinTables: TListBox
      AlignWithMargins = True
      Left = 3
      Top = 62
      Width = 187
      Height = 326
      Align = alClient
      ItemHeight = 13
      TabOrder = 1
      ExplicitLeft = 40
      ExplicitTop = 144
      ExplicitWidth = 121
      ExplicitHeight = 97
    end
  end
  object pnCommands: TPanel
    Left = 0
    Top = 391
    Width = 666
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 1
    ExplicitTop = 236
    ExplicitWidth = 474
    object Button1: TButton
      AlignWithMargins = True
      Left = 484
      Top = 3
      Width = 86
      Height = 27
      Action = actUseSQL
      Align = alRight
      TabOrder = 0
      ExplicitLeft = 304
    end
    object Button2: TButton
      AlignWithMargins = True
      Left = 576
      Top = 3
      Width = 87
      Height = 27
      Action = actCancel
      Align = alRight
      TabOrder = 1
      ExplicitLeft = 384
    end
    object Button3: TButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 118
      Height = 27
      Action = actDemoSelect
      Align = alLeft
      TabOrder = 2
    end
  end
  object mmSqlPreview: TMemo
    AlignWithMargins = True
    Left = 288
    Top = 8
    Width = 194
    Height = 142
    TabStop = False
    BevelKind = bkFlat
    BorderStyle = bsNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Consolas'
    Font.Style = []
    Lines.Strings = (
      'mmSqlPreview')
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object ActionList1: TActionList
    Left = 232
    Top = 72
    object actUseSQL: TAction
      Caption = 'actUseSQL'
      OnExecute = actUseSQLExecute
      OnUpdate = actUseSQLUpdate
    end
    object actCancel: TAction
      Caption = 'actCancel'
      OnExecute = actCancelExecute
    end
    object actDemoSelect: TAction
      Caption = 'actDemoSelect'
      OnExecute = actDemoSelectExecute
    end
    object actMainTableSelected: TAction
      Caption = 'actMainTableSelected'
      OnExecute = actMainTableSelectedExecute
    end
    object actJoinTableSelected: TAction
      Caption = 'actJoinTableSelected'
      OnExecute = actJoinTableSelectedExecute
    end
  end
end
