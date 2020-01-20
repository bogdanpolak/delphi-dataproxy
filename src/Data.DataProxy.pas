{* ------------------------------------------------------------------------
 * ♥
 * ♥  Delphi DataSetProxy component - wrapper for Delphi TDataSet
 * ♥
 * Home: https://github.com/bogdanpolak/delphi-dataproxy
 *
 * Classes:
 *   1. TGenericDataSetProxy - base class for the wrapper
 *   2. TDatasetProxy - inherited from TGenericDataSetProxy adding ForEach
 *   3. TDataProxyFactory - TDasetProxy and derived clasess factory
 *  ----------------------------------------------------------------------- * }
unit Data.DataProxy;

interface

uses
  System.Classes,
  System.SysUtils,
  Data.DB;

type
  TDataSetProxy = class(TComponent)
  private const
    Version = '0.9';
  protected
    fDataSet: TDataSet;
    procedure ConnectFields; virtual; abstract;
    procedure SetDataSet(aDataSet: TDataSet);
  public
    function WithDataSet(aDataSet: TDataSet): TDataSetProxy;
    function Open: TDataSetProxy;
    procedure ForEach(OnElem: TProc);
    // ----------------
    // TDataSet wrapped methods:
    procedure Append;
    procedure AppendRecord(const Values: array of const);
    procedure BindToDataSource(DataSource: TDataSource);
    procedure Cancel;
    function ConstructDataSource(AOwner: TComponent): TDataSource;
    procedure Close;
    function ControlsDisabled: Boolean;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
    procedure Delete;
    procedure DisableControls;
    procedure Edit;
    procedure EnableControls;
    procedure First;
    procedure Insert;
    procedure InsertRecord(const Values: array of const);
    function IsEmpty: Boolean;
    procedure Last;
    function Eof: Boolean;
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean;
    function Lookup(const KeyFields: string; const KeyValues: Variant;
      const ResultFields: string): Variant;
    procedure Next;
    procedure Post;
    procedure Prior;
    procedure Refresh;
    function UpdateStatus: TUpdateStatus;
  end;

implementation

// * --------------------------------------------------------------------
// * TGenericDataSetProxy
// * --------------------------------------------------------------------

procedure TDataSetProxy.Append;
begin
  fDataSet.Append;
end;

procedure TDataSetProxy.AppendRecord(const Values: array of const);
begin
  fDataSet.AppendRecord(Values);
end;

procedure TDataSetProxy.BindToDataSource(DataSource: TDataSource);
begin
  DataSource.DataSet := fDataSet;
end;

procedure TDataSetProxy.Cancel;
begin
  fDataSet.Cancel;
end;

procedure TDataSetProxy.Close;
begin
  fDataSet.Close;
end;

function TDataSetProxy.ConstructDataSource(AOwner: TComponent): TDataSource;
begin
  Result := TDataSource.Create(AOwner);
  Result.DataSet := fDataSet;
end;

function TDataSetProxy.ControlsDisabled: Boolean;
begin
  Result := fDataSet.ControlsDisabled;
end;

function TDataSetProxy.CreateBlobStream(Field: TField;
  Mode: TBlobStreamMode): TStream;
begin
  Result := fDataSet.CreateBlobStream(Field, Mode);
end;

procedure TDataSetProxy.Delete;
begin
  fDataSet.Delete;
end;

procedure TDataSetProxy.DisableControls;
begin
  fDataSet.DisableControls;
end;

procedure TDataSetProxy.Edit;
begin
  fDataSet.Edit;
end;

procedure TDataSetProxy.EnableControls;
begin
  fDataSet.EnableControls;
end;

function TDataSetProxy.Eof: Boolean;
begin
  Result := fDataSet.Eof;
end;

procedure TDataSetProxy.First;
begin
  fDataSet.First;
end;

procedure TDataSetProxy.Insert;
begin
  fDataSet.Insert;
end;

procedure TDataSetProxy.InsertRecord(const Values: array of const);
begin
  fDataSet.InsertRecord(Values);
end;

function TDataSetProxy.IsEmpty: Boolean;
begin
  Result := fDataSet.IsEmpty;
end;

procedure TDataSetProxy.Last;
begin
  fDataSet.Last;
end;

function TDataSetProxy.Locate(const KeyFields: string; const KeyValues: Variant;
  Options: TLocateOptions): Boolean;
begin
  Result := fDataSet.Locate(KeyFields, KeyValues, Options);
end;

function TDataSetProxy.Lookup(const KeyFields: string; const KeyValues: Variant;
  const ResultFields: string): Variant;
begin
  Result := fDataSet.Lookup(KeyFields, KeyValues, ResultFields);
end;

procedure TDataSetProxy.Next;
begin
  fDataSet.Next;
end;

procedure TDataSetProxy.Post;
begin
  fDataSet.Post;
end;

procedure TDataSetProxy.Prior;
begin
  fDataSet.Prior;
end;

procedure TDataSetProxy.Refresh;
begin
  fDataSet.Refresh;
end;

function TDataSetProxy.UpdateStatus: TUpdateStatus;
begin
  Result := fDataSet.UpdateStatus;
end;


// * --------------------------------------------------------------------
// * Extra methods (dedicated for Proxy)
// * --------------------------------------------------------------------

procedure TDataSetProxy.SetDataSet(aDataSet: TDataSet);
begin
  fDataSet := aDataSet;
  if fDataSet.Active then
    ConnectFields;
end;

function TDataSetProxy.WithDataSet(aDataSet: TDataSet): TDataSetProxy;
begin
  SetDataSet(aDataSet);
  Result := Self;
end;

function TDataSetProxy.Open: TDataSetProxy;
begin
  fDataSet.Open;
  ConnectFields;
  Result := Self;
end;

procedure TDataSetProxy.ForEach(OnElem: TProc);
var
  Bookmark: TBookmark;
begin
  Self.DisableControls;
  try
    Bookmark := fDataSet.GetBookmark;
    try
      Self.First;
      while not fDataSet.Eof do
      begin
        OnElem();
        fDataSet.Next;
      end;
    finally
      if fDataSet.BookmarkValid(Bookmark) then
        fDataSet.GotoBookmark(Bookmark);
      fDataSet.FreeBookmark(Bookmark);
    end;
  finally
    fDataSet.EnableControls;
  end;
end;

end.
