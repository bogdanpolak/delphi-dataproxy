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
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 374
    Top = 44
    Width = 5
    Height = 411
    ExplicitLeft = 345
    ExplicitTop = 0
    ExplicitHeight = 362
  end
  object ListBox1: TListBox
    AlignWithMargins = True
    Left = 3
    Top = 47
    Width = 371
    Height = 405
    Margins.Right = 0
    Style = lbOwnerDrawFixed
    Align = alLeft
    ItemHeight = 24
    TabOrder = 0
    ExplicitTop = 3
    ExplicitHeight = 449
  end
  object Memo1: TMemo
    AlignWithMargins = True
    Left = 379
    Top = 47
    Width = 399
    Height = 405
    Margins.Left = 0
    Align = alClient
    Lines.Strings = (
      'Book details (select book)')
    ScrollBars = ssVertical
    TabOrder = 1
    ExplicitLeft = 339
  end
  object FlowPanel1: TFlowPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 775
    Height = 38
    Align = alTop
    AutoSize = True
    Caption = ' '
    TabOrder = 2
    object btnPhase1: TButton
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 101
      Height = 30
      Align = alLeft
      Caption = 'Phase 1'
      TabOrder = 0
      OnClick = btnPhase1Click
    end
    object btnPhase2: TButton
      AlignWithMargins = True
      Left = 111
      Top = 4
      Width = 101
      Height = 30
      HelpType = htKeyword
      Caption = 'Phase 2'
      TabOrder = 1
    end
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Database=./books.sdb'
      'DriverID=SQLite')
    Left = 304
    Top = 24
  end
end
