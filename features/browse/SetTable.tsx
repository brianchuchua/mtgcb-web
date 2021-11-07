import Grid from '@material-ui/core/Grid';
import Table from '@material-ui/core/Table';
import TableBody from '@material-ui/core/TableBody';
import TableCell from '@material-ui/core/TableCell';
import TableContainer from '@material-ui/core/TableContainer';
import TableHead from '@material-ui/core/TableHead';
import TableRow from '@material-ui/core/TableRow';
import TableSortLabel from '@material-ui/core/TableSortLabel';
import Typography from '@material-ui/core/Typography';
import { Dispatch, SetStateAction, useMemo } from 'react';
import { useSortBy, useTable } from 'react-table';
import styled from 'styled-components';
import { PriceTypes } from './browseSlice';
import GalleryControls from './GalleryControls';
import { Set } from './SetBox';
import titleCase from './util/titleCase';

interface SetTableProps {
  sets: Set[];
  page: number;
  first: number;
  skip: number;
  setSkip: Dispatch<SetStateAction<number>>;
  setFirst: Dispatch<SetStateAction<number>>;
  setPage: Dispatch<SetStateAction<number>>;
  totalResults: number;
  priceType: PriceTypes;
}

const SetTable: React.FC<SetTableProps> = ({ sets, first, skip, page, totalResults, setSkip, setFirst, setPage, priceType }) => {
  const atLeastOneSetToShow = totalResults > 0;

  const setsTableColumns = useMemo(
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
        accessor: 'code',
        Header: 'Code',
      },
      {
        accessor: 'cardCount',
        Header: 'Card Count',
      },
      {
        accessor: 'category',
        Header: 'Category',
      },
      {
        accessor: 'setType',
        Header: 'Type',
        Cell: ({ cell: { value } }) => titleCase(value),
      },
      {
        accessor: 'releasedAt',
        Header: 'Release Date',
        Cell: ({ cell: { value } }) => value?.slice(0, 10),
      },
    ],
    []
  );

  const setsTableData = useMemo(() => sets, [sets]);

  const setsTable = useTable(
    {
      columns: setsTableColumns,
      data: setsTableData,
      initialState: { hiddenColumns: ['id'] },
    },
    useSortBy
  );

  const { getTableProps, getTableBodyProps, headerGroups, rows, allColumns, prepareRow } = setsTable;

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
          key: 'code',
          label: 'Code',
          getToggleHiddenProps: allColumns[2].getToggleHiddenProps,
        },
        {
          key: 'cardCount',
          label: 'Card Count',
          getToggleHiddenProps: allColumns[3].getToggleHiddenProps,
        },
        {
          key: 'category',
          label: 'Category',
          getToggleHiddenProps: allColumns[4].getToggleHiddenProps,
        },
        {
          key: 'setType',
          label: 'Type',
          getToggleHiddenProps: allColumns[5].getToggleHiddenProps,
        },
        {
          key: 'releasedAt_utc',
          label: 'Release Date',
          getToggleHiddenProps: allColumns[6].getToggleHiddenProps,
        },
      ],
    },
  ];

  return atLeastOneSetToShow ? (
    <>
      <GalleryControls
        items={sets}
        first={first}
        page={page}
        setFirst={setFirst}
        setPage={setPage}
        setSkip={setSkip}
        skip={skip}
        totalResults={totalResults}
        settingGroups={settingGroups}
        galleryType="sets"
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
                    <TableCell {...cell.getCellProps()}>{cell.render('Cell')}</TableCell>
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

export default SetTable;
