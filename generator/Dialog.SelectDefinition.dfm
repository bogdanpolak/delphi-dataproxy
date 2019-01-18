object DialogSelectDefinition: TDialogSelectDefinition
  Left = 0
  Top = 0
  Caption = 'DialogSelectDefinition'
  ClientHeight = 355
  ClientWidth = 203
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
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 197
    Height = 349
    Align = alClient
    Caption = 'FireDAC Connection Definition'
    TabOrder = 0
    object btnSelect: TButton
      AlignWithMargins = True
      Left = 5
      Top = 319
      Width = 187
      Height = 25
      Align = alBottom
      Caption = 'btnSelect'
      TabOrder = 0
    end
    object ListBox1: TListBox
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 187
      Height = 295
      Align = alClient
      ItemHeight = 13
      TabOrder = 1
      OnDblClick = ListBox1DblClick
      ExplicitLeft = 7
    end
  end
end
