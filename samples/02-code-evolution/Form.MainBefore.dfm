object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 455
  ClientWidth = 781
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 374
    Top = 0
    Width = 5
    Height = 455
    ExplicitLeft = 345
    ExplicitHeight = 362
  end
  object ListBox1: TListBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 371
    Height = 449
    Margins.Right = 0
    Style = lbOwnerDrawFixed
    Align = alLeft
    ItemHeight = 24
    TabOrder = 0
  end
  object Memo1: TMemo
    AlignWithMargins = True
    Left = 379
    Top = 3
    Width = 399
    Height = 449
    Margins.Left = 0
    Align = alClient
    Lines.Strings = (
      'Book details (select book)')
    ScrollBars = ssVertical
    TabOrder = 1
    ExplicitLeft = 350
    ExplicitWidth = 386
    ExplicitHeight = 356
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'ConnectionDef=SQLite_Books')
    Connected = True
    Left = 304
    Top = 24
  end
  object fdqBooks: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      
        'SELECT ISBN, Title, Authors, Status, ReleseDate, Pages, Price, C' +
        'urrency FROM {id Books}')
    Left = 304
    Top = 80
  end
end
