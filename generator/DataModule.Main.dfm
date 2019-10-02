object DataModule1: TDataModule1
  OldCreateOrder = False
  Height = 360
  Width = 330
  object FDConnection1: TFDConnection
    Params.Strings = (
      'ConnectionDef=IB_Demo')
    LoginPrompt = False
    Left = 40
    Top = 24
  end
  object FDQuery1: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      
        'SELECT OrderID,CustomerID,EmployeeID,OrderDate,RequiredDate,Ship' +
        'pedDate,ShipVia,Freight FROM {id Orders}')
    Left = 40
    Top = 80
  end
  object FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink
    Left = 232
    Top = 136
  end
  object FDPhysFBDriverLink1: TFDPhysFBDriverLink
    Left = 232
    Top = 56
  end
  object FDPhysPgDriverLink1: TFDPhysPgDriverLink
    Left = 232
    Top = 256
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 232
    Top = 96
  end
  object FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink
    Left = 232
    Top = 296
  end
end
