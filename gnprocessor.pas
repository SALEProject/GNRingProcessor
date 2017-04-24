unit GNProcessor;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, bitVariants, bitDataSource, DateUtils, DOM, XMLRead, XMLWrite,
 db, math;

const cns_int_ID_Ring: integer = 1;
      cns_int_ID_Asset: integer = 1;
      cns_int_ID_Bursary: integer = 2;

type TOrderDirection = (odir_Buy, odir_Sell);
     TOrderMatch = record
                    ID_OrderMatch: integer;
                    Orders: array of integer;
                    NodeData: pointer;//TTreeNode;

                    marked: boolean;
                   end;

     TOrderRec = record
                  ID_Order: integer;
                  ID_Agency: integer;
                  ID_Broker: integer;
                  ID_Client: integer;
                  OrderDate: TDateTime;
                  Direction: TOrderDirection;
                  Quantity: double;
                  Price: double;
                  StartDeliveryDate: TDateTime;
                  EndDeliveryDate: TDateTime;
                  CombinationsAccepted: boolean;
                  ID_GNType: integer;

                  matches: array of TOrderMatch;

                  NodeData: pointer;//TTreeNode;
                  row: integer;
                 end;
     TOrderRecArray = array of TOrderRec;

type TGNProcessor = class
                    private
                     FStatusFileName: shortstring;
                     FDataSource: TbitDataSource;

                     FMinDeliveryDate: TDateTime;
                     FMaxDeliveryDate: TDateTime;

                     function GetOrdersCount: integer;

                    public
                     function FindOrder(ID_Order: integer): integer;
                     function FindOrderbyNodeData(NodeData: pointer): integer;

                    public
                     Orders: array of TOrderRec;

                     constructor Create(StatusFileName: shortstring);
                     destructor Destroy; override;

                     procedure ReadStatus;
                     procedure WriteStatus;

                     function AddOrder(ID_Order: integer; ID_Agency: integer; ID_Broker: integer; ID_Client: integer; OrderDate: TDateTime;
                                       Direction: TOrderDirection; Quantity: double; Price: double;
                                       StartDeliveryDate: TDateTime; EndDeliveryDate: TDateTime;
                                       CombinationsAccepted: boolean; ID_GNType: integer): integer;
                     function RemoveOrder(ID_Order: integer): integer;

                     function FindMatch(ID_Order: integer; const match: array of integer): integer; overload;
                     function FindMatch(ID_Order: integer; ID_OrderMatch: integer): integer; overload;
                     function AddMatch(ID_Order: integer; const match: array of integer): integer;
                     procedure RemoveMatch(ID_Order: integer; ID_OrderMatch: integer);
                     function UnmarkMatch(ID_Order: integer; index: integer): boolean;

                     procedure Process;

                     property StatusFileName: shortstring read FStatusFileName write FStatusFileName;
                     property DataSource: TbitDataSource read FDataSource write FDataSource;
                     property OrdersCount: integer read GetOrdersCount;
                     property MinDeliveryDate: TDateTime read FMinDeliveryDate write FMinDeliveryDate;
                     property MaxDeliveryDate: TDateTime read FMaxDeliveryDate write FMaxDeliveryDate;

                    public //  db section
                     procedure Refresh_Orders;
                     function GetRingSession(ID_Ring: integer): integer;
                     function GetAssetSession(ID_Asset: integer): integer;
                     function isAgreed(ID_Asset: integer; ID_Client: integer; ID_AgreedClient: integer): boolean;
                     procedure InsertMatches;
                     procedure DeleteMarkedMatches;
                     function ExecuteTransaction(ID_RingSession, ID_AssetSession, ID_BuyOrder, ID_SellOrder: integer;
                                                 Quantity: double; Price: double;
                                                 StartDeliveryDate: TDateTime; EndDeliveryDate: TDateTime): integer;
                     procedure AddToJournal(Operation: string; ID_Broker: integer; ID_Agency: integer; ID_Client: integer;
                                            ID_Order: integer; Quantity: double; Price: double);
                     procedure SetNotification(ID_Broker, ID_Client, ID_Transation: integer; Quantity, Price: double);
                    end;

implementation

 function TGNProcessor.GetOrdersCount: integer;
  begin
   result:= Length(Orders);
  end;

 function TGNProcessor.FindOrder(ID_Order: integer): integer;
  var b: boolean;
      i: integer;
  begin
   result:= -1;
   b:= false;
   i:= -1;
   while not b and (i < Length(Orders) - 1) do
    begin
     i += 1;
     if Orders[i].ID_Order = ID_Order then b:= true;
    end;

   if b then result:= i;
  end;

 function TGNProcessor.FindOrderbyNodeData(NodeData: pointer): integer;
  var b: boolean;
      i: integer;
  begin
   result:= -1;
   b:= false;
   i:= -1;
   while not b and (i < Length(Orders) - 1) do
    begin
     i += 1;
     if Orders[i].NodeData = NodeData then b:= true;
    end;

   if b then result:= i;
   end;

 constructor TGNProcessor.Create(StatusFileName: shortstring);
  begin
   FStatusFileName:= StatusFileName;
  end;

 destructor TGNProcessor.Destroy;
  begin
   inherited;
  end;

 function GetXMLExpression(Node: TDomNode): string;
  begin
   result:= '';

   if node.ChildNodes.Count = 1 then
    if node.ChildNodes.Item[0].NodeName = '#text' then
     result:= node.ChildNodes.Item[0].NodeValue;
  end;

 procedure TGNProcessor.ReadStatus;

  function Parse_Direction(s: string): TOrderDirection;
   begin
    result:= odir_Buy;
    if Trim(UpperCase(s)) = 'SELL' then result:= odir_Sell;
   end;

  function Parser_Order(node: TDOMNode): TOrderRec;
   var i: integer;
       xml_node: TDOMNode;
       node_name: shortstring;
   begin
    FillChar(result, sizeof(result), 0);

    for i:= 0 to node.ChildNodes.Count - 1 do
     begin
      xml_node:= node.ChildNodes.Item[i];
      node_name:= Trim(UpperCase(xml_node.NodeName));

      if node_name = 'ID_ORDER'             then result.ID_Order:= StrToInt(GetXMLExpression(xml_node));
      if node_name = 'ID_BROKER'            then result.ID_Broker:= StrToInt(GetXMLExpression(xml_node));
      if node_name = 'ID_CLIENT'            then result.ID_Client:= StrToInt(GetXMLExpression(xml_node));
      if node_name = 'ORDERDATE'            then result.OrderDate:= StrToDateTime(GetXMLExpression(xml_node));
      if node_name = 'DIRECTION'            then result.Direction:= Parse_Direction(GetXMLExpression(xml_node));
      if node_name = 'QUANTITY'             then result.Quantity:= StrToFloat(GetXMLExpression(xml_node));
      if node_name = 'PRICE'                then result.Price:= StrToFloat(GetXMLExpression(xml_node));
      if node_name = 'STARTDELIVERYDATE'    then result.StartDeliveryDate:= StrToDateTime(GetXMLExpression(xml_node));
      if node_name = 'ENDDELIVERYDATE'      then result.EndDeliveryDate:= StrToDateTime(GetXMLExpression(xml_node));
      if node_name = 'COMBINATIONSACCEPTED' then result.CombinationsAccepted:= StrToBool(GetXMLExpression(xml_node));

      xml_node:= nil;
     end;

   end;

  var filename: string;
      FS: TFileStream;
      doc: TXMLDocument;
      xml_node: TDOMNode;
      node_name: shortstring;
      i, k: integer;
  begin
   SetLength(Orders, 0);

   filename:= ParamStr(0);
   filename:= IncludeTrailingPathDelimiter(ExtractFilePath(filename)) + 'GNRingProcessor_status.xml';

   doc:= TXMLDocument.Create;
   try
    FS:= TFileStream.Create(filename, fmOpenRead);

    try
     ReadXMLFile(doc, FS);
    except
    end;
   finally
    FreeAndNil(FS);
   end;

   for i:= 0 to doc.DocumentElement.ChildNodes.Count - 1 do
    begin
     xml_node:= doc.DocumentElement.ChildNodes.Item[i];
     node_name:= Trim(UpperCase(xml_node.NodeName));

     if node_name = 'MINDELIVERYDATE' then FMinDeliveryDate:= StrToDateTime(GetXMLExpression(xml_node));
     if node_name = 'MAXDELIVERYDATE' then FMaxDeliveryDate:= StrToDateTime(GetXMLExpression(xml_node));
     if node_name = 'ORDER' then
      begin
       k:= Length(Orders);
       SetLength(Orders, k + 1);
       Orders[k]:= Parser_Order(xml_node);
      end;

     xml_node:= nil;
    end;

   FreeAndNil(doc);
  end;

 procedure TGNProcessor.WriteStatus;
  var filename: shortstring;
      ms: TMemoryStream;
      FS: TFileStream;
      doc: TXMLDocument;
      doc_node, node_order, node_matches, node_match, node: TDOMElement;
      node_matchorders, node_matchorder: TDOMElement;
      i, j, k: integer;
  begin
   try
    doc:= TXMLDocument.Create;

    doc_node:= doc.CreateElement('GNRingProcessor');
    doc.AppendChild(doc_node);

    node:= doc.CreateElement('MinDeliveryDate');
    node.AppendChild(doc.CreateTextNode(DateTimeToStr(FMinDeliveryDate)));
    doc_node.AppendChild(node);

    node:= doc.CreateElement('MaxDeliveryDate');
    node.AppendChild(doc.CreateTextNode(DateTimeToStr(FMaxDeliveryDate)));
    doc_node.AppendChild(node);

    for i:= 0 to Length(Orders) - 1 do
     begin
      node_order:= doc.CreateElement('Order');
      doc_node.AppendChild(node_order);

      node:= doc.CreateElement('ID_Order');
      node.AppendChild(doc.CreateTextNode(IntToStr(Orders[i].ID_Order)));
      node_order.AppendChild(node);

      node:= doc.CreateElement('ID_Broker');
      node.AppendChild(doc.CreateTextNode(IntToStr(Orders[i].ID_Broker)));
      node_order.AppendChild(node);

      node:= doc.CreateElement('ID_Client');
      node.AppendChild(doc.CreateTextNode(IntToStr(Orders[i].ID_Client)));
      node_order.AppendChild(node);

      node:= doc.CreateElement('OrderDate');
      node.AppendChild(doc.CreateTextNode(DateTimeToStr(Orders[i].OrderDate)));
      node_order.AppendChild(node);

      node:= doc.CreateElement('Direction');
      case Orders[i].Direction of
       odir_Buy : node.AppendChild(doc.CreateTextNode('BUY'));
       odir_Sell: node.AppendChild(doc.CreateTextNode('SELL'));
      end;
      node_order.AppendChild(node);

      node:= doc.CreateElement('Quantity');
      node.AppendChild(doc.CreateTextNode(FloatToStr(Orders[i].Quantity)));
      node_order.AppendChild(node);

      node:= doc.CreateElement('Price');
      node.AppendChild(doc.CreateTextNode(FloatToStr(Orders[i].Price)));
      node_order.AppendChild(node);

      node:= doc.CreateElement('StartDeliveryDate');
      node.AppendChild(doc.CreateTextNode(DateTimeToStr(Orders[i].StartDeliveryDate)));
      node_order.AppendChild(node);

      node:= doc.CreateElement('EndDeliveryDate');
      node.AppendChild(doc.CreateTextNode(DateTimeToStr(Orders[i].EndDeliveryDate)));
      node_order.AppendChild(node);

      node:= doc.CreateElement('CombinationsAccepted');
      node.AppendChild(doc.CreateTextNode(BoolToStr(Orders[i].CombinationsAccepted, true)));
      node_order.AppendChild(node);

      node_matches:= doc.CreateElement('OrderMatches');
      node_order.AppendChild(node_matches);
      for j:= 0 to Length(Orders[i].matches) - 1 do
       begin
        node_match:= doc.CreateElement('OrderMatch');
        node_matches.AppendChild(node_match);

        node:= doc.CreateElement('ID_OrderMatch');
        node.AppendChild(doc.CreateTextNode(IntToStr(Orders[i].matches[j].ID_OrderMatch)));
        node_match.AppendChild(node);

        node_matchorders:= doc.CreateElement('Orders');
        node_match.AppendChild(node_matchorders);

        for k:= 0 to Length(Orders[i].matches[j].Orders) - 1 do
         begin
          node_matchorder:= doc.CreateElement('Order');
          node_matchorder.AppendChild(doc.CreateTextNode(IntToStr(Orders[i].matches[j].Orders[k])));
          node_matchorders.AppendChild(node_matchorder);
         end;
       end;
     end;

    ms:= TMemoryStream.Create;
    WriteXMLFile(doc, ms);
   finally
    doc.Free;
   end;

   if ms = nil then Exit;
   if ms.Size = 0 then Exit;

   try
    FS:= nil;
    try
     FS:= TFileStream.Create(FStatusFileName, fmCreate or fmShareDenyNone);

     ms.Seek(0, soFromBeginning);
     FS.CopyFrom(ms, ms.Size);
    except
    end;
   finally
    FreeAndNil(FS);
    FreeAndNil(ms);
   end;
  end;

 function TGNProcessor.AddOrder(ID_Order: integer; ID_Agency: integer; ID_Broker: integer; ID_Client: integer; OrderDate: TDateTime;
                                 Direction: TOrderDirection; Quantity: double; Price: double;
                                 StartDeliveryDate: TDateTime; EndDeliveryDate: TDateTime;
                                 CombinationsAccepted: boolean; ID_GNType: integer): integer;
  var k: integer;

  begin
   k:= Length(Orders);
   SetLength(Orders, k + 1);
   Orders[k].ID_Order:= ID_Order;
   Orders[k].ID_Agency:= ID_Agency;
   Orders[k].ID_Broker:= ID_Broker;
   Orders[k].ID_Client:= ID_Client;
   Orders[k].OrderDate:= OrderDate;
   Orders[k].Direction:= Direction;
   Orders[k].Quantity:= Quantity;
   Orders[k].Price:= Price;
   Orders[k].StartDeliveryDate:= StartDeliveryDate;
   Orders[k].EndDeliveryDate:= EndDeliveryDate;
   Orders[k].CombinationsAccepted:= CombinationsAccepted;
   Orders[k].ID_GNType:= ID_GNType;

   result:= k;
  end;

 function TGNProcessor.RemoveOrder(ID_Order: integer): integer;
  var i: integer;
  begin
   result:= FindOrder(ID_Order);
   if result = -1 then Exit;

   for i:= result to Length(Orders) - 2 do Orders[i]:= Orders[i + 1];
   SetLength(Orders, Length(Orders) - 1);
  end;

 function TGNProcessor.FindMatch(ID_Order: integer; const match: array of integer): integer;
  var b, bb: boolean;
      idx, i, j, k: integer;
  begin
   result:= -1;
   idx:= FindOrder(ID_Order);
   if idx = -1 then Exit;

   b:= false;
   i:= -1;
   while not b and (i < Length(Orders[idx].matches) - 1) do
    begin
     i += 1;

     if Length(Orders[idx].matches[i].Orders) = Length(match) then
      begin
       b:= true;
       for j:= 0 to Length(match) - 1 do
        begin
         bb:= false;
         k:= -1;
         while not bb and (k < Length(Orders[idx].matches[i].Orders) - 1) do
          begin
           k += 1;
           if Orders[idx].matches[i].Orders[k] = match[j] then bb:= true;
          end;

         if not bb then b:= false;
        end;
      end;
    end;

   if b then result:= i;
  end;

 function TGNProcessor.FindMatch(ID_Order: integer; ID_OrderMatch: integer): integer;
  var idx, i: integer;
      b: boolean;
  begin
   result:= -1;
   idx:= FindOrder(ID_Order);
   if idx = -1 then Exit;

   b:= false;
   i:= -1;
   while not b and (i < Length(Orders[idx].matches) - 1) do
    begin
     i += 1;

     if Orders[idx].matches[i].ID_OrderMatch = ID_OrderMatch then b:= true;
    end;

   if b then result:= i;
  end;

 function TGNProcessor.AddMatch(ID_Order: integer; const match: array of integer): integer;
  var idx, i, k: integer;
  begin
   result:= -1;
   idx:= FindOrder(ID_Order);
   if idx = -1 then Exit;

   k:= Length(Orders[idx].matches);
   SetLength(Orders[idx].matches, k + 1);

   FillChar(Orders[idx].matches[k], sizeof(TOrderMatch), 0);
   SetLength(Orders[idx].matches[k].Orders, Length(match));
   for i:= 0 to Length(match) - 1 do Orders[idx].matches[k].Orders[i]:= match[i];
   result:= k;
  end;

 procedure TGNProcessor.RemoveMatch(ID_Order: integer; ID_OrderMatch: integer);
  var idx, k, i: integer;
  begin
   idx:= FindOrder(ID_Order);
   if idx = -1 then Exit;

   k:= FindMatch(ID_Order, ID_OrderMatch);
   if k = -1 then Exit;

   for i:= k to Length(Orders[idx].matches) - 2 do Orders[idx].matches[i]:= Orders[idx].matches[i + 1];
   SetLength(Orders[idx].matches, Length(Orders[idx].matches) - 1);
  end;

 function TGNProcessor.UnmarkMatch(ID_Order: integer; index: integer): boolean;
  var idx: integer;
  begin
   result:= false;
   idx:= FindOrder(ID_Order);
   if idx = -1 then Exit;

   if (index < 0) or (index >= Length(Orders[idx].matches)) then Exit;
   Orders[idx].matches[index].marked:= false;
   result:= true;
  end;

 procedure TGNProcessor.Process;

  function FindPerfectMatch(order: TOrderRec; shortlist: array of TOrderRec): integer;
   var b: boolean;
       i: integer;
   begin
    result:= -1;

    b:= false;
    i:= -1;
    while not b and (i < Length(shortlist) - 1) do
     begin
      i += 1;

      if (order.StartDeliveryDate = shortlist[i].StartDeliveryDate)
      and (order.EndDeliveryDate = shortlist[i].EndDeliveryDate)
      and (order.Quantity = shortlist[i].Quantity)
      and (order.Price = shortlist[i].Price) then b:= true;
     end;

    if b then result:= i;
   end;

  function FindIntersection(order: TOrderRec; shortlist: array of TOrderRec): integer;
   var b: boolean;
       i, idx_bestmatch: integer;
   begin
    idx_bestmatch:= -1;
    result:= -1;

    i:= -1;
    while (i < Length(shortlist) - 1) do
     begin
      i += 1;
      b:= false;

      if (order.StartDeliveryDate = shortlist[i].StartDeliveryDate)
      and (order.EndDeliveryDate = shortlist[i].EndDeliveryDate)
      and (order.Quantity = shortlist[i].Quantity) then b:= true;

      if b then
       case order.Direction of
        odir_Buy : if shortlist[i].Price > order.Price then b:= false;
        odir_Sell: if shortlist[i].Price < order.Price then b:= false;
       end;

      if b then
       begin
        if idx_bestmatch = -1 then idx_bestmatch:= i
        else if shortlist[idx_bestmatch].OrderDate < shortlist[i].OrderDate then idx_bestmatch:= i;
       end;
     end;

    result:= idx_bestmatch;
   end;

  function FindBestPrice(order: TOrderRec; shortlist: array of TOrderRec): integer;
   var b: boolean;
       i: integer;
       best_price: double;
       best_choice: integer;
   begin
    result:= -1;
    if Length(shortlist) = 0 then Exit;

    b:= false;
    best_choice:= -1;
    case order.Direction of
     odir_Buy : best_price:= order.Price * 1.2;
     odir_Sell: best_price:= order.Price * 0.8;
    end;

    for i:= 0 to Length(shortlist) - 1 do
     case order.Direction of
      odir_Buy : if (order.StartDeliveryDate = shortlist[i].StartDeliveryDate)
                 and (order.EndDeliveryDate = shortlist[i].EndDeliveryDate)
                 and (order.Quantity = shortlist[i].Quantity)
                 and (shortlist[i].Price < best_price) then
                  begin
                   b:= true;
                   best_price:= shortlist[i].Price;
                   best_choice:= i;
                  end;
      odir_Sell: if (order.StartDeliveryDate = shortlist[i].StartDeliveryDate)
                 and (order.EndDeliveryDate = shortlist[i].EndDeliveryDate)
                 and (order.Quantity = shortlist[i].Quantity)
                 and (shortlist[i].Price > best_price) then
                  begin
                   b:= true;
                   best_price:= shortlist[i].Price;
                   best_choice:= i;
                  end;
     end;

    if b then result:= best_choice;
   end;

  function FindBestQuantity(order: TOrderRec; shortlist: array of TOrderRec): integer;
   var b: boolean;
       i: integer;
       best_price, best_quantity: double;
       best_choice: integer;
   begin
    result:= -1;
    if Length(shortlist) = 0 then Exit;

    b:= false;
    best_choice:= -1;
    best_quantity:= order.Quantity * 0.8;
    case order.Direction of
     odir_Buy : best_price:= order.Price * 1.2;
     odir_Sell: best_price:= order.Price * 0.8;
    end;

    for i:= 0 to Length(shortlist) - 1 do
     case order.Direction of
      odir_Buy : if (order.StartDeliveryDate = shortlist[i].StartDeliveryDate)
                 and (order.EndDeliveryDate = shortlist[i].EndDeliveryDate)
                 and (abs(shortlist[i].Quantity - order.Quantity) < abs(order.Quantity - best_quantity))
                 and (shortlist[i].Price < best_price) then
                  begin
                   b:= true;
                   best_price:= shortlist[i].Price;
                   best_quantity:= shortlist[i].Quantity;
                   best_choice:= i;
                  end;
      odir_Sell: if (order.StartDeliveryDate = shortlist[i].StartDeliveryDate)
                 and (order.EndDeliveryDate = shortlist[i].EndDeliveryDate)
                 and (abs(shortlist[i].Quantity - order.Quantity) < abs(order.Quantity - best_quantity))
                 and (shortlist[i].Price > best_price) then
                  begin
                   b:= true;
                   best_price:= shortlist[i].Price;
                   best_quantity:= shortlist[i].Quantity;
                   best_choice:= i;
                  end;
     end;

    if b then result:= best_choice;
   end;

  function FindCombinations(order: TOrderRec; shortlist: array of TOrderRec): TOrderRecArray;
   var solution, best: TOrderRecArray;
       dt_start: TDateTime;
       AverageQuantity: double;
       DaysLength: integer;

   function Check: boolean;
    var Quantity, Price: double;
        i: integer;
    begin
     result:= false;
     if Length(solution) = 0 then Exit;
     result:= true;
     if solution[0].StartDeliveryDate <> order.StartDeliveryDate then result:= false;
     if solution[Length(solution) - 1].EndDeliveryDate <> order.EndDeliveryDate then result:= false;

     Quantity:= 0;
     Price:= 0;
     for i:= 0 to Length(solution) - 1 do
      begin
       Quantity += solution[i].Quantity;
       Price += solution[i].Quantity * solution[i].Price;
      end;
     Price:= Price / Quantity;
     Price:= round(Price * 100) / 100; //  round to only two decimals

     if Quantity <> order.Quantity then result:= false;
     case order.Direction of
      odir_Buy : if Price > order.Price then result:= false;
      odir_Sell: if Price < order.Price then result:= false;
     end;
     //if Price <> order.Price then result:= false;
    end;

   function Validate: boolean;
    var i, k, dlen: integer;
        Quantity, avgqty, dqty: double;
    begin
     k:= Length(solution) - 1;
     result:= true;

     dlen:= round(int(solution[k].EndDeliveryDate)) - round(int(solution[k].StartDeliveryDate));// + 1;qwe
     if dlen <= 0 then begin result:= false; Exit end;
     avgqty:= solution[k].Quantity / dlen;
     avgqty:= round(avgqty * 10000);
     avgqty:= avgqty / 10000;

     dqty:= abs(avgqty - AverageQuantity);
     if dqty > 0.001 then result:= false;
     //if avgqty <> AverageQuantity then result:= false;
     if result then for i:= 0 to k - 1 do if solution[i].ID_Order = solution[k].ID_Order then result:= false;
     if result and (k = 0) then if solution[k].StartDeliveryDate <> dt_start then result:= false;
     if result and (k > 0) then if solution[k].StartDeliveryDate <> solution[k - 1].EndDeliveryDate{ + 1qwe} then result:= false;
     if result then if solution[k].EndDeliveryDate > order.EndDeliveryDate then result:= false;

     if result then
      begin
       Quantity:= 0;
       for i:= 0 to k do Quantity += solution[i].Quantity;
       if Quantity > order.Quantity then result:= false;
      end;
    end;

   procedure Search;
    var k, i: integer;
    begin
     if Check then
      begin
       if (Length(solution) < Length(best)) or (Length(best) = 0) then
        begin
         SetLength(best, Length(solution));
         for i:= 0 to Length(solution) - 1 do best[i]:= solution[i];
        end;
       Exit;
      end;

     k:= Length(solution);
     SetLength(solution, k + 1);

     for i:= 0 to Length(shortlist) - 1 do
      begin
       solution[k]:= shortlist[i];
       if Validate then Search;
      end;

     SetLength(solution, k);
    end;

   begin
    FillChar(result, sizeof(result), 0);
    SetLength(solution, 0);
    SetLength(best, 0);
    if Length(shortlist) = 0 then Exit;
    DaysLength:= round(int(order.EndDeliveryDate)) - round(int(order.StartDeliveryDate));// + 1;qwe
    if DaysLength <= 0 then Exit;
    AverageQuantity:= order.Quantity / DaysLength;
    AverageQuantity:= round(AverageQuantity * 10000);
    AverageQuantity:= AverageQuantity / 10000;

    dt_start:= order.StartDeliveryDate;
    Search;

    result:= best;
   end;

 var i, j, k: integer;
     order, o: TOrderRec;
     shortlist, match: array of TOrderRec;
     matchids: array of integer;
     b: boolean;
     idx_perfect, idx_intersection, idx_bestprice, idx_bestquantity: integer;
     ID_RingSession: integer;
     ID_AssetSession: integer;
     PriceSettlement: double;
     ID_Transaction: integer;
 begin
  ID_RingSession:= GetRingSession(cns_int_ID_Ring);
  ID_AssetSession:= GetAssetSession(cns_int_ID_Asset);
  if (ID_RingSession <= 0) or (ID_AssetSession <= 0) then Exit;

  //  mark all previous matches for deletion
  for i:= 0 to Length(Orders) - 1 do
   for j:= 0 to Length(Orders[i].matches) - 1 do
    Orders[i].matches[j].marked:= true;

  i:= 0;
  //for i:= 0 to Length(Orders) - 1 do
  while i < Length(Orders) do
   begin
    order:= Orders[i];
    SetLength(shortlist, 0);

    //---------------------------------------------------------------------------
    //  filter the other orders and generate a subset of possibilities
    for j:= 0 to Length(Orders) - 1 do
     if i <> j then
      begin
       o:= Orders[j];

       if order.ID_GNType <> o.ID_GNType then Continue; //  this condition prevents matching between different types of Gas
       if order.Direction = o.Direction then Continue; //  cannot match orders of the same direction
       if order.ID_Client = o.ID_Client then Continue; //  orders cannot come from the same client
       if order.StartDeliveryDate > o.StartDeliveryDate then Continue;
       if order.EndDeliveryDate < o.EndDeliveryDate then Continue;
       if not order.CombinationsAccepted and (order.StartDeliveryDate <> o.StartDeliveryDate)
       and (order.EndDeliveryDate <> o.EndDeliveryDate) then Continue; //  orders that do not accept
                                                                       //  combinations require exact matching of
                                                                       //  delivery interval
       //  check if order o is from an accepted partner
       if isAgreed(cns_int_ID_Asset, order.ID_Client, o.ID_Client) then
        begin
         //  add to shortlist
         k:= Length(shortlist);
         SetLength(shortlist, k + 1);
         shortlist[k]:= o;
        end;
      end;

    if Length(shortlist) = 0 then begin i += 1; Continue; end; //  no variants were found

    //---------------------------------------------------------------------------
    //  search for a perfect match in order of dates
    idx_perfect:= FindPerfectMatch(order, shortlist);
    idx_intersection:= FindIntersection(order, shortlist);
    idx_bestprice:= FindBestPrice(order, shortlist);
    idx_bestquantity:= FindBestQuantity(order, shortlist);
    if idx_perfect > -1 then
     begin
      //  search and add/update match in the matches list
      k:= FindMatch(order.ID_Order, [shortlist[idx_perfect].ID_Order]);
      if k = -1 then AddMatch(order.ID_Order, [shortlist[idx_perfect].ID_Order])
      else UnmarkMatch(order.ID_Order, k);

      k:= FindMatch(shortlist[idx_perfect].ID_Order, [order.ID_Order]);
      if k = -1 then AddMatch(shortlist[idx_perfect].ID_Order, [order.ID_Order])
      else UnmarkMatch(shortlist[idx_perfect].ID_Order, k);

      //  try to execute transaction
      case order.Direction of
       odir_Buy : ID_Transaction:= ExecuteTransaction(ID_RingSession, ID_AssetSession, order.ID_Order, shortlist[idx_perfect].ID_Order, order.Quantity, order.Price, order.StartDeliveryDate, order.EndDeliveryDate);
       odir_Sell: ID_Transaction:= ExecuteTransaction(ID_RingSession, ID_AssetSession, shortlist[idx_perfect].ID_Order, order.ID_Order, order.Quantity, order.Price, order.StartDeliveryDate, order.EndDeliveryDate);
      end;
      SetNotification(order.ID_Broker, order.ID_Client, ID_Transaction, order.Quantity, order.Price);
      SetNotification(shortlist[idx_perfect].ID_Broker, shortlist[idx_perfect].ID_Client, ID_Transaction, order.Quantity, order.Price);
      AddToJournal('transaction', order.ID_Broker, order.ID_Agency, order.ID_Client, order.ID_Order, order.Quantity, order.Price);

      //  remove orders from lists
      RemoveOrder(order.ID_Order); i -= 1;
      if RemoveOrder(shortlist[idx_perfect].ID_Order) <= i then i -= 1;
     end
    else if idx_intersection > -1 then
     begin
      //  search and add/update match in the matches list
      k:= FindMatch(order.ID_Order, [shortlist[idx_intersection].ID_Order]);
      if k = -1 then AddMatch(order.ID_Order, [shortlist[idx_intersection].ID_Order])
      else UnmarkMatch(order.ID_Order, k);

      k:= FindMatch(shortlist[idx_intersection].ID_Order, [order.ID_Order]);
      if k = -1 then AddMatch(shortlist[idx_intersection].ID_Order, [order.ID_Order])
      else UnmarkMatch(shortlist[idx_intersection].ID_Order, k);

      PriceSettlement:= order.Price;
      if shortlist[idx_intersection].OrderDate > order.OrderDate then PriceSettlement:= shortlist[idx_intersection].Price;

      //  try to execute transaction
      case order.Direction of
       odir_Buy : ExecuteTransaction(ID_RingSession, ID_AssetSession, order.ID_Order, shortlist[idx_intersection].ID_Order, order.Quantity, PriceSettlement, order.StartDeliveryDate, order.EndDeliveryDate);
       odir_Sell: ExecuteTransaction(ID_RingSession, ID_AssetSession, shortlist[idx_intersection].ID_Order, order.ID_Order, order.Quantity, PriceSettlement, order.StartDeliveryDate, order.EndDeliveryDate);
      end;
      SetNotification(order.ID_Broker, order.ID_Client, ID_Transaction, order.Quantity, PriceSettlement);
      SetNotification(shortlist[idx_perfect].ID_Broker, shortlist[idx_perfect].ID_Client, ID_Transaction, order.Quantity, PriceSettlement);
      AddToJournal('transaction', order.ID_Broker, order.ID_Agency, order.ID_Client, order.ID_Order, order.Quantity, PriceSettlement);

      //  remove orders from lists
      RemoveOrder(order.ID_Order); i -= 1;
      if RemoveOrder(shortlist[idx_intersection].ID_Order) <= i then i -= 1;
     end
    else if idx_bestprice > -1 then
     begin
      k:= FindMatch(order.ID_Order, [shortlist[idx_bestprice].ID_Order]);
      if k = -1 then AddMatch(order.ID_Order, [shortlist[idx_bestprice].ID_Order])
      else UnmarkMatch(order.ID_Order, k);

      k:= FindMatch(shortlist[idx_bestprice].ID_Order, [order.ID_Order]);
      if k = -1 then AddMatch(shortlist[idx_bestprice].ID_Order, [order.ID_Order])
      else UnmarkMatch(shortlist[idx_bestprice].ID_Order, k);
     end
    else if idx_bestquantity > -1 then
     begin
      k:= FindMatch(order.ID_Order, [shortlist[idx_bestquantity].ID_Order]);
      if k = -1 then AddMatch(order.ID_Order, [shortlist[idx_bestquantity].ID_Order])
      else UnmarkMatch(order.ID_Order, k);

      k:= FindMatch(shortlist[idx_bestquantity].ID_Order, [order.ID_Order]);
      if k = -1 then AddMatch(shortlist[idx_bestquantity].ID_Order, [order.ID_Order])
      else UnmarkMatch(shortlist[idx_bestquantity].ID_Order, k);
     end;

    if (idx_perfect = -1) and (idx_intersection = -1) and order.CombinationsAccepted then
     begin
      //  search for a combination of orders
      match:= FindCombinations(order, shortlist);
      if Length(match) > 0 then
       begin
        SetLength(matchids, Length(match));
        for j:= 0 to Length(matchids) - 1 do matchids[j]:= match[j].ID_Order;

        k:= FindMatch(order.ID_Order, matchids);
        if k = -1 then AddMatch(order.ID_Order, matchids)
        else UnmarkMatch(order.ID_Order, k);
       end;
     end;

    i += 1;
   end;

  DeleteMarkedMatches;
  InsertMatches;

  //Refresh_TreeOrders;
  //Paint_Orders;
 end;




 //-----------------------------------------------------------------------------
 //  DB Section
 //-----------------------------------------------------------------------------

 procedure TGNProcessor.Refresh_Orders;
  var ds, ds_OrderMatches, ds_OrderMatchDetails: TDataSet;
      ID_Order, ID_Agency, ID_Broker, ID_Client, ID_OrderMatch, ID_OrderMatchDetail: integer;
      i, j, k: integer;
      OrderDate: TDateTime;
      Direction: TOrderDirection;
      Quantity, Price: double;
      dt_start, dt_end: TDateTime;
      CombinationsAccepted: boolean;
      ID_GNType: integer;
      //node, node_match, node_matchdet: TTreeNode;
  begin
   if FDataSource = nil then Exit;

   SetLength(Orders, 0);
   //tv_Orders.Items.Clear;

   ds:= nil;
   try
    ds:= FDataSource['brm'].Query('SELECT ' +
                                  'CAST(MIN(RGN."StartDeliveryDate") AS DATETIME) AS "MinDeliveryDate", ' +
                                  'CAST(MAX(RGN."EndDeliveryDate") AS DATETIME) AS "MaxDeliveryDate" ' +
                                  'FROM "Orders" O ' +
                                  'LEFT JOIN "OrderDetails_RGN" RGN ON (O."ID" = RGN."ID_Order")' +
                                  'WHERE (O."isActive" = 1) AND (O."ID_Asset" = ' + IntToStr(cns_int_ID_Asset) + ')');
    if ds = nil then Exit;
    if ds.IsEmpty then Exit;
    MinDeliveryDate:= ds.FieldByName('MinDeliveryDate').AsDateTime;
    MaxDeliveryDate:= ds.FieldByName('MaxDeliveryDate').AsDateTime;
   finally
    FreeAndNil(ds);
   end;

   try
    ds:= FDataSource['brm'].Query('SELECT O.*, ' +
                                  'CAST(RGN."StartDeliveryDate" AS DATETIME) AS "StartDeliveryDate", ' +
                                  'CAST(RGN."EndDeliveryDate" AS DATETIME) AS "EndDeliveryDate", ' +
                                  'RGN."CombinationsAccepted", RGN."ID_GNType" ' +
                                  'FROM "Orders" O ' +
                                  'LEFT JOIN "OrderDetails_RGN" RGN ON (O."ID" = RGN."ID_Order") ' +
                                  'WHERE (O."isActive" = 1) AND (O."ID_Asset" = ' + IntToStr(cns_int_ID_Asset) + ') ' +
                                  'ORDER BY O."Date"');
    if ds = nil then Exit;

    ds.First;
    while not ds.Eof do
     begin
      ID_Order:= ds.FieldByName('ID').AsInteger;
      ID_Agency:= ds.FieldByName('ID_Agency').AsInteger;
      ID_Broker:= ds.FieldByName('ID_Broker').AsInteger;
      ID_Client:= ds.FieldByName('ID_Client').AsInteger;
      OrderDate:= ds.FieldByName('Date').AsDateTime;

      case ds.FieldByName('Direction').AsBoolean of
       false: Direction:= odir_Buy;
       true : Direction:= odir_Sell;
      end;

      Quantity:= ds.FieldByName('Quantity').AsFloat;
      Price:= ds.FieldByName('Price').AsFloat;
      dt_start:= ds.FieldByName('StartDeliveryDate').AsDateTime;
      dt_end:= ds.FieldByName('EndDeliveryDate').AsDateTime;
      CombinationsAccepted:= ds.FieldByName('CombinationsAccepted').AsBoolean;
      ID_GNType:= ds.FieldByName('ID_GNType').AsInteger;

      //node:= tv_Orders.Items.AddChild(nil, IntToStr(ID_Order));

      i:= AddOrder(ID_Order, ID_Agency, ID_Broker, ID_Client, OrderDate, Direction, Quantity, Price, dt_start, dt_end, CombinationsAccepted, ID_GNType);
      //Orders[i].node:= node;

      ds_OrderMatches:= nil;
      try
       ds_OrderMatches:= FDataSource['brm'].Query('SELECT * FROM "OrderMatches" WHERE "ID_Order" = ' + IntToStr(ID_Order));
       if ds_OrderMatches = nil then continue;

       ds_OrderMatches.First;
       while not ds_OrderMatches.Eof do
        begin
         ID_OrderMatch:= ds_OrderMatches.FieldByName('ID').AsInteger;

         //node_match:= tv_Orders.Items.AddChild(node, 'ID_OrderMatch: ' + IntToStr(ID_OrderMatch));

         j:= Length(Orders[i].matches);
         SetLength(Orders[i].matches, j + 1);
         Orders[i].matches[j].ID_OrderMatch:= ID_OrderMatch;
         //Orders[i].matches[j].node:= node_match;

         ds_OrderMatchDetails:= nil;
         try
          ds_OrderMatchDetails:= FDataSource['brm'].Query('SELECT * FROM "OrderMatchDetails" WHERE "ID_OrderMatch" = ' + IntToStr(ID_OrderMatch));
          if ds_OrderMatchDetails = nil then continue;

          ds_OrderMatchDetails.First;
          while not ds_OrderMatchDetails.Eof do
           begin
            ID_OrderMatchDetail:= ds_OrderMatchDetails.FieldByName('ID').AsInteger;

            //node_matchdet:= tv_Orders.Items.AddChild(node_match, 'ID_Order: ' + IntToStr(ds_OrderMatchDetails.FieldByName('ID_Order').AsInteger));

            k:= Length(Orders[i].matches[j].Orders);
            SetLength(Orders[i].matches[j].Orders, k + 1);
            Orders[i].matches[j].Orders[k]:= ds_OrderMatchDetails.FieldByName('ID_Order').AsInteger;

            ds_OrderMatchDetails.Next;
           end;
         finally
          FreeAndNil(ds_OrderMatchDetails);
         end;

         ds_OrderMatches.Next;
        end;
      finally
       FreeAndNil(ds_OrderMatches);
      end;

      ds.Next;
     end;
   finally
    FreeAndNil(ds);
   end;
  end;

 function TGNProcessor.GetRingSession(ID_Ring: integer): integer;
  var ds_session: TDataSet;
  begin
   result:= 0;
   if FDataSource = nil then Exit;

   ds_session:= nil;
   try
    ds_session:= FDataSource['brm'].Query(Format('SELECT TOP 1 * FROM "RingSessions" WHERE "ID_Ring" = %d ORDER BY "Date" DESC', [ID_Ring]));
    if ds_session = nil then Exit;
    if ds_session.IsEmpty then Exit;

    if int(ds_session.FieldByName('Date').AsDateTime) <> int(Now) then Exit;
    if (ds_session.FieldByName('Status').AsString <> 'Opened') then Exit;

    result:= ds_session.FieldByName('ID').AsInteger;
   finally
    FreeAndNil(ds_session);
   end;
  end;

 function TGNProcessor.GetAssetSession(ID_Asset: integer): integer;
  var ds_session: TDataSet;
  begin
   result:= 0;
   if FDataSource = nil then Exit;

   ds_session:= nil;
   try
    ds_session:= FDataSource['brm'].Query(Format('SELECT TOP 1 * FROM "AssetSessions" WHERE "ID_Asset" = %d ORDER BY "Date" DESC', [ID_Asset]));
    if ds_session = nil then Exit;
    if ds_session.IsEmpty then Exit;

    if int(ds_session.FieldByName('Date').AsDateTime) <> int(Now) then Exit;
    if (ds_session.FieldByName('Status').AsString <> 'Opened') then Exit;

    result:= ds_session.FieldByName('ID').AsInteger;
   finally
    FreeAndNil(ds_session);
   end;
  end;

 function TGNProcessor.isAgreed(ID_Asset: integer; ID_Client: integer; ID_AgreedClient: integer): boolean;
  var ds: TDataSet;
  begin
   result:= true;

   try
    ds:= FDataSource['brm'].Query(Format('SELECT * FROM "AgreedClients" WHERE ("ID_Asset" = %d) AND ("ID_Client" = %d) AND ("ID_AgreedClient" = %d)', [ID_Asset, ID_Client, ID_AgreedClient]));
    if ds = nil then Exit;
    if ds.IsEmpty then result:= true
    else
     begin
      result:= ds.FieldByName('isAgreed').AsBoolean;
      if not result then if not ds.FieldByName('isApproved').AsBoolean then result:= true;
     end;
   finally
    FreeAndNil(ds);
   end;

   if result then
    try
     ds:= FDataSource['brm'].Query(Format('SELECT * FROM "AgreedClients" WHERE ("ID_Asset" = %d) AND ("ID_Client" = %d) AND ("ID_AgreedClient" = %d)', [ID_Asset, ID_AgreedClient, ID_Client]));
     if ds = nil then Exit;
     if ds.IsEmpty then result:= true
     else
      begin
       result:= ds.FieldByName('isAgreed').AsBoolean;
       if not result then if not ds.FieldByName('isApproved').AsBoolean then result:= true;
      end;
    finally
     FreeAndNil(ds);
    end;
  end;

 procedure TGNProcessor.InsertMatches;
  var i, j, k: integer;
      ds_identity: TDataSet;
  begin
   if FDataSource = nil then Exit;

   for i:= 0 to Length(Orders) - 1 do
    begin
     for j:= 0 to Length(Orders[i].matches) - 1 do
      if (Orders[i].matches[j].ID_OrderMatch = 0) and not Orders[i].matches[j].marked then
       begin
        FDataSource['brm'].Execute(Format('INSERT INTO "OrderMatches" ("ID_Order") VALUES (%d)', [Orders[i].ID_Order]));
        ds_identity:= FDataSource['brm'].Query('SELECT SCOPE_IDENTITY() AS "Identity"');
        Orders[i].matches[j].ID_OrderMatch:= ds_identity.FieldByName('Identity').AsInteger;

        for k:= 0 to Length(Orders[i].matches[j].Orders) - 1 do
         FDataSource['brm'].Execute(Format('INSERT INTO "OrderMatchDetails" ("ID_OrderMatch", "ID_Order") VALUES (%d, %d)', [Orders[i].matches[j].ID_OrderMatch, Orders[i].matches[j].Orders[k]]));
       end;
    end;
  end;

 procedure TGNProcessor.DeleteMarkedMatches;
  var i, j: integer;
  begin
   if FDataSource = nil then Exit;

   for i:= 0 to Length(Orders) - 1 do
    begin
     j:= 0;
     while j < Length(Orders[i].matches) do
      begin
       if Orders[i].matches[j].marked then
        begin
         FDataSource['brm'].Execute(Format('DELETE FROM "OrderMatchDetails" WHERE "ID_OrderMatch" = %d', [Orders[i].matches[j].ID_OrderMatch]));
         FDataSource['brm'].Execute(Format('DELETE FROM "OrderMatches" WHERE "ID" = %d', [Orders[i].matches[j].ID_OrderMatch]));
         RemoveMatch(Orders[i].ID_Order, Orders[i].matches[j].ID_OrderMatch);
         j -= 1;
        end;

       j += 1;
      end;
    end;
  end;

 function TGNProcessor.ExecuteTransaction(ID_RingSession, ID_AssetSession, ID_BuyOrder, ID_SellOrder: integer;
                                          Quantity: double; Price: double;
                                          StartDeliveryDate: TDateTime; EndDeliveryDate: TDateTime): integer;
  var vl_params: TbitVariantList;
      idx_buy, idx_sell: integer;
      ID_Transaction: integer;
      ds_identity: TDataSet;
  begin
   if FDataSource = nil then Exit;

   idx_buy:= FindOrder(ID_BuyOrder);
   idx_sell:= FindOrder(ID_SellOrder);
   if (idx_buy = -1) or (idx_sell = -1) then Exit;

   vl_params:= TbitVariantList.Create;
   vl_params.Add('@prm_ID_RingSession').AsInteger:= ID_RingSession;
   vl_params.Add('@prm_ID_AssetSession').AsInteger:= ID_AssetSession;
   vl_params.Add('@prm_ID_Ring').AsInteger:= cns_int_ID_Ring;
   vl_params.Add('@prm_ID_Asset').AsInteger:= cns_int_ID_Asset;
   vl_params.Add('@prm_ID_BuyOrder').AsInteger:= ID_BuyOrder;
   vl_params.Add('@prm_ID_SellOrder').AsInteger:= ID_SellOrder;
   vl_params.Add('@prm_Quantity').AsDouble:= Quantity;
   vl_params.Add('@prm_Price').AsDouble:= Price;

   FDataSource['brm'].Execute('INSERT INTO "Transactions" ' +
                              '("ID_Ring", "ID_RingSession", "ID_Asset", "ID_AssetSession", "ID_BuyOrder", "ID_SellOrder", "Quantity", "Price") ' +
                              'VALUES ' +
                              '(@prm_ID_Ring, @prm_ID_RingSession, @prm_ID_Asset, @prm_ID_AssetSession, @prm_ID_BuyOrder, @prm_ID_SellOrder, @prm_Quantity, @prm_Price)', vl_params);
   ds_identity:= FDataSource['brm'].Query('SELECT SCOPE_IDENTITY() AS "Identity"');
   ID_Transaction:= ds_identity.FieldByName('Identity').AsInteger;

   vl_params.Clear;
   vl_params.Add('@prm_ID_Transaction').AsInteger:= ID_Transaction;
   vl_params.Add('@prm_StartDeliveryDate').AsDateTime:= StartDeliveryDate;
   vl_params.Add('@prm_EndDeliveryDate').AsDateTime:= EndDeliveryDate;
   FDataSource['brm'].Execute('INSERT INTO "TransactionDetails_RGN" ' +
                              '("ID_Transaction", "StartDeliveryDate", "EndDeliveryDate") ' +
                              'VALUES ' +
                              '(@prm_ID_Transaction, @prm_StartDeliveryDate, @prm_EndDeliveryDate)', vl_params);

   //  mark orders as transacted
   FDataSource['brm'].Execute(Format('UPDATE "Orders" SET "isTransacted" = 1, "isActive" = 0 WHERE "ID" = %d', [ID_BuyOrder]));
   FDataSource['brm'].Execute(Format('UPDATE "Orders" SET "isTransacted" = 1, "isActive" = 0 WHERE "ID" = %d', [ID_SellOrder]));

   FreeAndNil(vl_params);

   result:= ID_Transaction;
  end;

 procedure TGNProcessor.AddToJournal(Operation: string; ID_Broker: integer; ID_Agency: integer; ID_Client: integer;
                                     ID_Order: integer; Quantity: double; Price: double);
  var str_sql: string;
  begin
   str_sql:= 'INSERT INTO "Journal" ("Date", "Operation", "ID_User", "ID_Broker", "ID_Agency", "ID_Client", ' +
             '"ID_Bursary", "ID_Ring", "ID_Asset", "ID_Order", "Quantity", "Price") VALUES ' +
             '(GetDate(), ''%s'', 0, %d, %d, %d, %d, %d, %d, %d, %f, %f)';
   str_sql:= Format(str_sql, [Operation, ID_Broker, ID_Agency, ID_Client, cns_int_ID_Bursary, cns_int_ID_Ring, cns_int_ID_Asset, ID_Order, Quantity, Price ]);

   FDataSource['brm'].Execute(str_sql);
  end;

 procedure TGNProcessor.SetNotification(ID_Broker, ID_Client, ID_Transation: integer; Quantity, Price: double);
  var ds: TDataSet;
      ID_RecipientAgency, ID_RecipientUser: integer;
      Subject, Body: string;
  begin
   ds:= FDataSource['brm'].Query(Format('SELECT A.* FROM "Brokers" B ' +
                                        'LEFT JOIN "Agencies" A ON (B."ID_Agency" = A."ID") ' +
                                        'LEFT JOIN "Clients" C ON (A."ID" = C."ID_Agency") ' +
                                        'WHERE (B."ID" = %d) AND (C."ID" = %d)', [ID_Broker, ID_Client]));
   if ds = nil then Exit;
   if ds.IsEmpty then Exit;
   ID_RecipientAgency:= ds.FieldByName('ID').AsInteger;
   FreeAndNil(ds);

   ds:= FDataSource['brm'].Query(Format('SELECT U.* FROM "Brokers" B ' +
                                        'LEFT JOIN "Users" U ON (B."ID_User" = U."ID") ' +
                                        'WHERE B."ID_Agency" = %d', [ID_RecipientAgency]));
   if ds = nil then Exit;
   if ds.IsEmpty then Exit;
   ds.First;
   while not ds.EOF do
    begin
     Subject:= Format('Tranzactionare %fx%f', [Quantity, Price]);
     Body:= Subject;
     ID_RecipientUser:= ds.FieldByName('ID').AsInteger;
     FDataSource['brm'].Execute(Format('INSERT INTO "Notifications" ' +
                                       '("ID_Bursary", "ID_SenderUser", "ID_SenderAgency", "ID_RecipientUser", "ID_RecipientAgency", "Subject", "Body", "BodyHTML") ' +
                                       'VALUES ' +
                                       '(%d, %d, %d, %d, %d, ''%s'', ''%s'', ''%s'')',
                                       [2, 0, 0, ID_RecipientUser, ID_RecipientAgency, Subject, Body, Body]));

     ds.Next;
    end;
  end;

end.

