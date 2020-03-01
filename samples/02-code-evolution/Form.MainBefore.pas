unit Form.MainBefore;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,

  Winapi.Windows, Winapi.Messages,

  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs, FireDAC.VCLUI.Wait, Data.DB, FireDAC.Comp.Client,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Comp.DataSet,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    ListBox1: TListBox;
    FDConnection1: TFDConnection;
    fdqBooks: TFDQuery;
    Splitter1: TSplitter;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

type
  TBook = class
  strict private
    FISBN: string;
    FTitle: String;
    FAuthors: TList<string>;
    FReleseDate: TDateTime;
    FIsPreciseReleseDate: boolean;
    FPrice: Currency;
    FPriceCurrency: string;
    FPages: integer;
  private
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure BuildAuhtorsList(const aAutlorsList: string);
    property ISBN: string read FISBN write FISBN;
    property Title: String read FTitle write FTitle;
    property Authors: TList<string> read FAuthors write FAuthors;
    property ReleseDate: TDateTime read FReleseDate write FReleseDate;
    property IsPreciseReleseDate: boolean read FIsPreciseReleseDate
      write FIsPreciseReleseDate;
    property Price: Currency read FPrice write FPrice;
    property PriceCurrency: string read FPriceCurrency write FPriceCurrency;
    property Pages: integer read FPages write FPages;
  end;

constructor TBook.Create;
begin
  FAuthors := TList<string>.Create;
end;

destructor TBook.Destroy;
begin
  FAuthors.Free;
  inherited;
end;

procedure TBook.BuildAuhtorsList(const aAutlorsList: string);
begin
end;

function ConvertReleaseDate(const aReleseDate: string;
  out isDatePrecise: boolean): TDateTime;
begin
  Result := 0;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ListBox1.Clear;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  aIndex: integer;
  aBookmark: TBookmark;
  aBookDataSet: TFDQuery;
  aBook: TBook;
  isDatePrecise: boolean;
  aBookCaption: string;
begin
  ListBox1.ItemIndex := -1;
  for aIndex := 0 to ListBox1.Items.Count-1 do
    ListBox1.Items.Objects[aIndex].Free;
  ListBox1.Clear;
  fdqBooks.Open();
  aBookDataSet := fdqBooks;
  aBookmark := aBookDataSet.GetBookmark;
  try
    aBookDataSet.DisableControls;
    try
      while not aBookDataSet.Eof do
      begin
        aBookCaption := aBookDataSet.FieldByName('ISBN').AsString + ' - ' +
          aBookDataSet.FieldByName('Title').AsString;
        aBook := TBook.Create;
        ListBox1.AddItem(aBookCaption, aBook);
        aBook.ISBN := aBookDataSet.FieldByName('ISBN').AsString;
        aBook.BuildAuhtorsList(aBookDataSet.FieldByName('Authors').AsString);
        aBook.Title := aBookDataSet.FieldByName('Title').AsString;
        aBook.ReleseDate := ConvertReleaseDate
          (aBookDataSet.FieldByName('ReleseDate').AsString, isDatePrecise);
        aBook.IsPreciseReleseDate := isDatePrecise;
        aBook.Price := aBookDataSet.FieldByName('Price').AsCurrency;
        aBook.PriceCurrency := aBookDataSet.FieldByName('Currency').AsString;
        aBookDataSet.Next;
      end;
    finally
      aBookDataSet.EnableControls;
    end
  finally
    aBookDataSet.FreeBookmark(aBookmark);
  end;
end;

end.
