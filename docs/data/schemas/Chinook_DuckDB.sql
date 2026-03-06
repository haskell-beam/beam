-- This script is adapted from
-- https://github.com/RandomFractals/duckdb-sql-tools/blob/main/data/chinook/duckdb/create.sql
-- to match the schema that is used for Postgres and Sqlite
drop table if exists Album;
drop table if exists Artist;
drop table if exists Customer;
drop table if exists Employee;
drop table if exists Genre;
drop table if exists Invoice;
drop table if exists InvoiceLine;
drop table if exists MediaType;
drop table if exists Playlist;
drop table if exists PlaylistTrack;
drop table if exists Track;
drop sequence if exists invoice_id_seq;

create table Album
(
  AlbumId integer not null,
  Title nvarchar(160) not null,
  ArtistId integer not null,
  constraint pk_album primary key (AlbumId)
);

create table Artist
(
  ArtistId integer not null,
  Name nvarchar(120),
  constraint pk_artist primary key (ArtistId)
);

create table Customer
(
  CustomerId integer not null,
  FirstName nvarchar(40) not null,
  LastName nvarchar(20) not null,
  Company nvarchar(80),
  Address nvarchar(70),
  City nvarchar(40),
  State nvarchar(40),
  Country nvarchar(40),
  PostalCode nvarchar(10),
  Phone nvarchar(24),
  Fax nvarchar(24),
  Email nvarchar(60) not null,
  SupportRepId integer,
  constraint pk_customer primary key (CustomerId)
);

create table Employee
(
  EmployeeId integer not null,
  LastName nvarchar(20) not null,
  FirstName nvarchar(20) not null,
  Title nvarchar(30),
  ReportsTo integer,
  BirthDate date,
  HireDate date,
  Address nvarchar(70),
  City nvarchar(40),
  State nvarchar(40),
  Country nvarchar(40),
  PostalCode nvarchar(10),
  Phone nvarchar(24),
  Fax nvarchar(24),
  Email nvarchar(60),
  constraint pk_employee primary key (EmployeeId)
);

create table Genre
(
  GenreId integer not null,
  Name nvarchar(120),
  constraint pk_genre primary key (GenreId)
);

create sequence invoice_id_seq start 1;

create table Invoice
(
  InvoiceId integer primary key default nextval('invoice_id_seq'),
  CustomerId integer not null,
  InvoiceDate date not null,
  BillingAddress nvarchar(70),
  BillingCity nvarchar(40),
  BillingState nvarchar(40),
  BillingCountry nvarchar(40),
  BillingPostalCode nvarchar(10),
  Total numeric(10,2) not null
);

create table InvoiceLine
(
  InvoiceLineId integer not null,
  InvoiceId integer not null,
  TrackId integer not null,
  UnitPrice numeric(10,2) not null,
  Quantity integer not null,
  constraint pk_invoice_line primary key (InvoiceLineId)
);

create table MediaType
(
  MediaTypeId integer not null,
  Name nvarchar(120),
  constraint pk_media_type primary key (MediaTypeId)
);

create table Playlist
(
  PlaylistId integer not null,
  Name nvarchar(120),
  constraint pk_playlist primary key (PlaylistId)
);

create table PlaylistTrack
(
  PlaylistId integer not null,
  TrackId integer not null,
  constraint pk_playlist_track primary key (PlaylistId, TrackId)
);

create table Track
(
  TrackId integer not null,
  Name nvarchar(200) not null,
  AlbumId integer,
  MediaTypeId integer not null,
  GenreId integer,
  Composer nvarchar(220),
  Milliseconds integer not null,
  Bytes integer,
  UnitPrice numeric(10,2) not null,
  constraint pk_track primary key (TrackId)
);

create index ifk_album_artist_id on Album (ArtistId);
create index ifk_customer_support_rep_id on Customer (SupportRepId);
create index ifk_employee_reports_to on Employee (ReportsTo);
create index ifk_invoice_customer_id on Invoice (CustomerId);
create index ifk_invoice_item_invoice_id on InvoiceLine (InvoiceId);
create index ifk_invoice_item_track_id on InvoiceLine (TrackId);
create index ifk_playlist_track_track_id on PlaylistTrack (TrackId);
create index ifk_track_album_id on Track (AlbumId);
create index ifk_track_genre_id on Track (GenreId);
create index ifk_track_media_type_id on Track (MediaTypeId);


-- Inserting just enough data for the documentation to build
INSERT INTO Customer (customerId, firstName, lastName, company, address, city, state, country, postalCode, phone, fax, email, supportRepId) VALUES
    (14, N'Mark', N'Philips', N'Telus', N'8210 111 ST NW', N'Edmonton', N'AB', N'Canada', N'T6G 2C7', N'+1 (780) 434-4554', N'+1 (780) 434-5565', N'mphilips12@shaw.ca', 5)