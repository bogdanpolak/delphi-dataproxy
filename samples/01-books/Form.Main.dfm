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
      Top = 18
      Width = 164
      Height = 39
      Align = alTop
      Caption = 'Load Books'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      AlignWithMargins = True
      Left = 5
      Top = 63
      Width = 164
      Height = 40
      Align = alTop
      Caption = 'Button2'
      TabOrder = 1
      WordWrap = True
      OnClick = Button2Click
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
    Left = 392
    Top = 24
  end
  object FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink
    Left = 392
    Top = 80
  end
end
