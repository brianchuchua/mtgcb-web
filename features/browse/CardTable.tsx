import Grid from '@material-ui/core/Grid';
import Table from '@material-ui/core/Table';
import TableBody from '@material-ui/core/TableBody';
import TableCell from '@material-ui/core/TableCell';
import TableContainer from '@material-ui/core/TableContainer';
import TableHead from '@material-ui/core/TableHead';
import TableRow from '@material-ui/core/TableRow';
import TableSortLabel from '@material-ui/core/TableSortLabel';
import Tooltip from '@material-ui/core/Tooltip';
import Typography from '@material-ui/core/Typography';
import { Dispatch, SetStateAction, useMemo } from 'react';
import { useSortBy, useTable } from 'react-table';
import styled from 'styled-components';
import Link from '../../components/Link';
import ManaCost, { sortByManaSymbols } from '../../components/symbols/mana/ManaCost';
import { PriceTypes } from './browseSlice';
import CardBox, { Card } from './CardBox';
import GalleryControls from './GalleryControls';
import { formatPrice } from './util/formatPrice';

interface CardTableProps {
  cards: Card[];
  page: number;
  first: number;
  skip: number;
  setSkip: Dispatch<SetStateAction<number>>;
  setFirst: Dispatch<SetStateAction<number>>;
  setPage: Dispatch<SetStateAction<number>>;
  totalResults: number;
  priceType: PriceTypes;
  collectionByCardId?: [
    {
      cardID: string;
      quantityReg: number;
      quantityFoil: number;
    }
  ];
  userId?: string;
  isShowingSingleSet?: boolean;
}

const CardTable: React.FC<CardTableProps> = ({
  cards,
  first,
  skip,
  page,
  totalResults,
  setSkip,
  setFirst,
  setPage,
  priceType,
  userId,
  collectionByCardId,
  isShowingSingleSet = false,
}) => {
  const atLeastOneCardToShow = totalResults > 0;

  const cardsTableColumns = useMemo(
    () => [
      {
        accessor: 'id',
        Header: 'MTG CB ID',
      },
      {
        accessor: 'name',
        Header: 'Name',
      },
      {
        accessor: 'set',
        Header: 'Set',
        Cell: ({ cell: { value } }) => (
          <Link href={userId ? `/collections/${userId}/sets/${value?.slug}` : `/browse/sets/${value?.slug}`} variant="body2">
            {value?.name ?? 'Unknown Set'}
          </Link>
        ),
        sortType: (a, b) => (a?.values?.set?.name ?? '').localeCompare(b?.values?.set?.name ?? ''),
      },
      {
        accessor: 'rarity',
        Header: 'Rarity',
        sortType: (rowA, rowB, columnId) => {
          const rankings = { mythic: 1, rare: 2, uncommon: 3, common: 4, special: 5 };
          const leftRanking = rankings[rowA.values[columnId]] ?? 6;
          const rightRanking = rankings[rowB.values[columnId]] ?? 6;
          return leftRanking - rightRanking;
        },
        Cell: ({ cell: { value } }) => (value ? value.charAt(0).toUpperCase() + value.slice(1) : value),
      },
      {
        accessor: 'manaCost',
        Header: 'Mana Cost',
        sortType: (rowA, rowB, columnId, desc) => {
          const leftConvertedManaCost = rowA.values.convertedManaCost ?? 0;
          const rightConvertedManaCost = rowB.values.convertedManaCost ?? 0;

          if (leftConvertedManaCost !== rightConvertedManaCost) {
            return leftConvertedManaCost - rightConvertedManaCost;
          }

          return sortByManaSymbols(rowA.values[columnId], rowB.values[columnId], desc);
        },
      },
      {
        accessor: 'convertedManaCost',
        Header: 'CMC',
        Cell: ({ cell: { value } }) => {
          const numberValue = Number(value);
          if (numberValue > 0 && numberValue < 1) {
            return numberValue.toFixed(1);
          }
          const integerValue = parseInt(value, 10);
          if (integerValue >= 1) {
            return parseInt(value, 10);
          }
          if (integerValue === 0) {
            return integerValue;
          }
          return value;
        },
      },
      {
        accessor: 'oracleTypeLine',
        Header: 'Type',
      },
      {
        accessor: 'collectorNumber',
        Header: 'Card #',
        sortMethod: (a: string, b: string) => String(a).localeCompare(String(b), 'en', { numeric: true, sensitivity: 'base' }),
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        Cell: ({ value }: any) => (value === '0' ? '' : value),
      },
      {
        accessor: 'quantityReg',
        Header: 'Qt.',
      },
      {
        accessor: 'quantityFoil',
        Header: 'Foil Qt.',
      },
      {
        accessor: 'market',
        Header: 'Price (Market)',
        Cell: ({ row: { original } }) => formatPrice(original, 'market'),
      },
      {
        accessor: 'low',
        Header: 'Price (Low)',
        Cell: ({ row: { original } }) => formatPrice(original, 'low'),
      },
      {
        accessor: 'average',
        Header: 'Price (Avg)',
        Cell: ({ row: { original } }) => formatPrice(original, 'average'),
      },
      {
        accessor: 'high',
        Header: 'Price (High)',
        Cell: ({ row: { original } }) => formatPrice(original, 'high'),
      },
      {
        accessor: 'foil',
        Header: 'Price (Foil)',
        Cell: ({ row: { original } }) => formatPrice(original, 'foil', false),
      },
      {
        accessor: 'tcgplayerId',
        Header: 'TCGPlayer ID',
      },
    ],
    []
  );

  const cardsTableData = useMemo(() => {
    if (!cards) {
      return [];
    }
    if (collectionByCardId) {
      return cards.map((card) => ({
        ...card,
        quantityReg: collectionByCardId?.[card.id]?.quantityReg ?? 0,
        quantityFoil: collectionByCardId?.[card.id]?.quantityFoil ?? 0,
      }));
    }

    return cards;
  }, [cards, collectionByCardId]);

  const cardsTable = useTable(
    {
      columns: cardsTableColumns,
      data: cardsTableData,
      initialState: {
        hiddenColumns: [
          'id',
          'convertedManaCost',
          'oracleTypeLine',
          'low',
          'average',
          'high',
          'tcgplayerId',
          isShowingSingleSet ? 'set' : '',
          userId ? '' : 'quantityReg',
          userId ? '' : 'quantityFoil',
        ],
      }, // TODO: Hide quantities if not in collector mode, see other table
    },
    useSortBy
  );

  const { getTableProps, getTableBodyProps, headerGroups, rows, allColumns, prepareRow } = cardsTable;

  const settingGroups = [
    {
      label: 'Show Columns',
      type: 'tableFilters',
      settings: [
        {
          key: 'id',
          label: 'MTG CB ID',
          getToggleHiddenProps: allColumns[0].getToggleHiddenProps,
        },
        {
          key: 'name',
          label: 'Name',
          getToggleHiddenProps: allColumns[1].getToggleHiddenProps,
        },
        {
          key: 'set',
          label: 'Set',
          getToggleHiddenProps: allColumns[2].getToggleHiddenProps,
        },
        {
          key: 'rarity',
          label: 'Rarity',
          getToggleHiddenProps: allColumns[3].getToggleHiddenProps,
        },
        {
          key: 'manaCost',
          label: 'Mana Cost',
          getToggleHiddenProps: allColumns[4].getToggleHiddenProps,
        },
        {
          key: 'convertedManaCost',
          label: 'CMC',
          getToggleHiddenProps: allColumns[5].getToggleHiddenProps,
        },
        {
          key: 'oracleTypeLine',
          label: 'Type',
          getToggleHiddenProps: allColumns[6].getToggleHiddenProps,
        },
        {
          key: 'collectorNumber',
          label: 'Collector Number',
          getToggleHiddenProps: allColumns[7].getToggleHiddenProps,
        },
        {
          key: 'quantityReg',
          label: 'Quantity (Regular)',
          getToggleHiddenProps: allColumns[8].getToggleHiddenProps,
        },
        {
          key: 'quantityFoil',
          label: 'Quantity (Foil)',
          getToggleHiddenProps: allColumns[9].getToggleHiddenProps,
        },
        {
          key: 'price.market',
          label: 'Price (Market)',
          getToggleHiddenProps: allColumns[10].getToggleHiddenProps,
        },
        {
          key: 'price.low',
          label: 'Price (Low)',
          getToggleHiddenProps: allColumns[11].getToggleHiddenProps,
        },
        {
          key: 'price.average',
          label: 'Price (Avg)',
          getToggleHiddenProps: allColumns[12].getToggleHiddenProps,
        },
        {
          key: 'price.high',
          label: 'Price (High)',
          getToggleHiddenProps: allColumns[13].getToggleHiddenProps,
        },
        {
          key: 'price.foil',
          label: 'Price (Foil)',
          getToggleHiddenProps: allColumns[14].getToggleHiddenProps,
        },
        {
          key: 'tcgplayerId',
          label: 'TCGPlayer ID',
          getToggleHiddenProps: allColumns[15].getToggleHiddenProps,
        },
      ],
    },
  ];

  return atLeastOneCardToShow ? (
    <>
      <GalleryControls
        items={cards}
        first={first}
        page={page}
        setFirst={setFirst}
        setPage={setPage}
        setSkip={setSkip}
        skip={skip}
        totalResults={totalResults}
        settingGroups={settingGroups}
        galleryType="cards"
      />
      <StyledTableContainer>
        <Table {...getTableProps()} size="small">
          <StyledTableHead>
            {headerGroups.map((headerGroup) => (
              <TableRow {...headerGroup.getHeaderGroupProps()}>
                {headerGroup.headers.map((column) => (
                  <StyledHeaderTableCell
                    {...column.getHeaderProps(
                      column.getSortByToggleProps({ title: '(Sorts only the current table page, not the entire search)' })
                    )}
                    style={column.id === 'market' ? { minWidth: '175px' } : {}}
                  >
                    {column.render('Header')} <TableSortLabel active={column.isSorted} direction={column.isSortedDesc ? 'desc' : 'asc'} />
                  </StyledHeaderTableCell>
                ))}
              </TableRow>
            ))}
          </StyledTableHead>
          <TableBody {...getTableBodyProps()}>
            {rows.map((row) => {
              prepareRow(row);
              return (
                <TableRow {...row.getRowProps()}>
                  {row.cells.map((cell) => (
                    <TableCell {...cell.getCellProps()}>
                      {cell.column.id === 'name' && (
                        <Tooltip
                          interactive
                          title={
                            <TooltipWrapper>
                              <CardBox
                                card={{
                                  id: row.values.id,
                                  name: row.values.name,
                                  set: { name: row.values.set?.name || '', slug: row.values.set?.slug || '' },
                                  low: row.values.low,
                                  average: row.values.average,
                                  high: row.values.high,
                                  foil: row.values.foil,
                                  market: row.values.market,
                                  tcgplayerId: row.values.tcgplayerId,
                                }}
                                priceType={priceType}
                              />
                            </TooltipWrapper>
                          }
                        >
                          <div>
                            <Link
                              href={generateCardUrl(row.values.tcgplayerId, row.values.name, row.values)}
                              target="_blank"
                              rel="noreferrer"
                            >
                              {cell.render('Cell')}
                            </Link>
                          </div>
                        </Tooltip>
                      )}
                      {cell.column.id === 'manaCost' && <ManaCost manaCost={row.values.manaCost} />}
                      {cell.column.id !== 'name' && cell.column.id !== 'manaCost' && cell.render('Cell')}
                    </TableCell>
                  ))}
                </TableRow>
              );
            })}
          </TableBody>
        </Table>
      </StyledTableContainer>
    </>
  ) : (
    <Grid container alignItems="center" justify="center">
      <Grid item>
        <Typography variant="h6">No results found -- try another search!</Typography>
      </Grid>
    </Grid>
  );
};

const StyledTableContainer = styled(TableContainer)(() => ({ marginTop: '5px' }));
const StyledTableHead = styled(TableHead)(() => ({ backgroundColor: '#424242', boxShadow: '2px' }));
const StyledHeaderTableCell = styled(TableCell)(() => ({ minWidth: '150px' }));
const TooltipWrapper = styled.div({
  minHeight: '488px',
  minWidth: '270px',
});

const generateCardUrl = (cardId: string | number, cardName: string, card: any) =>
  cardId
    ? `https://shop.tcgplayer.com/magic/product/productsearch?id=${cardId}&utm_campaign=affiliate&utm_medium=CTNBLDR&utm_source=CTNBLDR`
    : `https://www.tcgplayer.com/search/magic/product?productLineName=magic&q=${cardName}&utm_campaign=affiliate&utm_medium=CTNBLDR&utm_source=CTNBLDR`;
export default CardTable;
