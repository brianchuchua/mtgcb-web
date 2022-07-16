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
import { Set, SetSummary } from './SetBox';
import { browseTableColumns, collectionTableColumns } from './tables/setTableColumns';
import { browseHiddenColumns, browseSettingGroups, collectionHiddenColumns, collectionSettingGroups } from './tables/setTableSettings';

interface SetTableProps {
  sets: Set[];
  costsToPurchase?: SetSummary[];
  page: number;
  first: number;
  skip: number;
  setSkip: Dispatch<SetStateAction<number>>;
  setFirst: Dispatch<SetStateAction<number>>;
  setPage: Dispatch<SetStateAction<number>>;
  totalResults: number;
  priceType: PriceTypes;
  isCollectorMode?: boolean;
  userId?: string;
}

const SetTable: React.FC<SetTableProps> = ({
  sets,
  costsToPurchase,
  first,
  skip,
  page,
  totalResults,
  setSkip,
  setFirst,
  setPage,
  priceType,
  isCollectorMode = false,
  userId = null,
}) => {
  const atLeastOneSetToShow = totalResults > 0;

  const setsTableColumns = useMemo(() => (isCollectorMode ? collectionTableColumns(priceType, userId) : browseTableColumns(priceType)), [
    costsToPurchase,
    priceType,
  ]);

  const setsTableData = useMemo(() => {
    if (!sets) {
      return [];
    }
    const setsWithCostsToPurchase = [];
    if (costsToPurchase) {
      sets?.forEach((set) => {
        const costsToPurchaseInSet = costsToPurchase?.find((costs) => Number(costs.setId) === Number(set.id));
        const setWithCostToPurchase = { ...set, costsToPurchaseInSet };
        setsWithCostsToPurchase.push(setWithCostToPurchase);
      });
    } else {
      return sets;
    }
    return setsWithCostsToPurchase;
  }, [sets, costsToPurchase]);

  const setsTable = useTable(
    {
      columns: setsTableColumns,
      data: setsTableData,
      initialState: {
        hiddenColumns: isCollectorMode ? collectionHiddenColumns : browseHiddenColumns,
      },
    },
    useSortBy
  );

  const { getTableProps, getTableBodyProps, headerGroups, rows, allColumns, prepareRow } = setsTable;

  const settingGroups = isCollectorMode ? collectionSettingGroups(allColumns, priceType) : browseSettingGroups(allColumns, priceType);

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
                    style={column.id === 'costsToPurchaseInSet.percentageCollected' ? { minWidth: '275px' } : {}}
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
      <div style={{ marginTop: '20px' }}>
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
          isOnBottom
        />
      </div>
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
