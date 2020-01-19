object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 345
  ClientWidth = 695
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
  object Splitter1: TSplitter
    Left = 180
    Top = 0
    Width = 5
    Height = 345
    ExplicitLeft = 191
    ExplicitHeight = 318
  end
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 174
    Height = 339
    Align = alLeft
    Caption = 'GroupBox1'
    TabOrder = 0
    object Button1: TButton
      AlignWithMargins = True
      Left = 5
      Top = 103
      Width = 164
      Height = 39
      Align = alTop
      Caption = 'Read Books -> ListBox'
      TabOrder = 1
      OnClick = Button1Click
      ExplicitTop = 18
    end
    object Button2: TButton
      AlignWithMargins = True
      Left = 5
      Top = 148
      Width = 164
      Height = 40
      Align = alTop
      Caption = 'Button2'
      TabOrder = 2
      WordWrap = True
      OnClick = Button2Click
      ExplicitTop = 63
    end
    object GroupBox2: TGroupBox
      AlignWithMargins = True
      Left = 5
      Top = 27
      Width = 164
      Height = 70
      Margins.Top = 12
      Align = alTop
      Caption = 'GroupBox2'
      TabOrder = 0
      object rbtnSqlDataset: TRadioButton
        Tag = 2
        AlignWithMargins = True
        Left = 17
        Top = 41
        Width = 142
        Height = 17
        Margins.Left = 15
        Align = alTop
        Caption = 'SQL DataSet (SQLite)'
        TabOrder = 0
        OnClick = rbtnSqlDatasetClick
        ExplicitLeft = 24
        ExplicitTop = 48
        ExplicitWidth = 113
      end
      object rbtnMemoryDataset: TRadioButton
        Tag = 1
        AlignWithMargins = True
        Left = 17
        Top = 18
        Width = 142
        Height = 17
        Margins.Left = 15
        Align = alTop
        Caption = 'Memory DataSet'
        Checked = True
        TabOrder = 1
        TabStop = True
        OnClick = rbtnMemoryDatasetClick
        ExplicitLeft = 19
      end
    end
  end
  object ListBox1: TListBox
    AlignWithMargins = True
    Left = 188
    Top = 3
    Width = 504
    Height = 339
    Align = alClient
    ItemHeight = 13
    TabOrder = 1
    OnClick = ListBox1Click
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'ConnectionDef=SQLite_Books')
    LoginPrompt = False
    Left = 392
    Top = 24
  end
  object FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink
    Left = 392
    Top = 80
  end
end
