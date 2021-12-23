import LinearProgress from '@material-ui/core/LinearProgress';
import Typography from '@material-ui/core/Typography';
import Link from '../../../components/Link';
import { formatter } from '../util/formatPrice';
import titleCase from '../util/titleCase';

export const collectionTableColumns = (priceType) => [
  {
    accessor: 'id',
    Header: 'MTG CB ID',
  },
  {
    accessor: 'code',
    Header: 'Code',
  },
  {
    accessor: 'name',
    Header: 'Name',
    Cell: (cell) => {
      const setName = cell?.row?.values?.name ?? 'Unknown Set';
      const setSlug = cell?.row?.values?.slug ?? 'unknown-set';
      return (
        <Link href={`/browse/sets/${setSlug}`} variant="body2">
          {setName}
        </Link>
      );
    },
    sortType: (a, b) => (a?.values?.set?.name ?? '').localeCompare(b?.values?.set?.name ?? ''),
  },
  {
    accessor: 'costsToPurchaseInSet.percentageCollected',
    Header: 'Progress',
    Cell: ({ cell: { value, row } }) => {
      const uniqueCardsCollected = row?.values?.['costsToPurchaseInSet.uniquePrintingsCollectedInSet'];
      const totalCards = row?.values?.cardCount;
      return (
        // TODO: Make this color orange once the set is complete
        <div style={{ position: 'relative' }}>
          <LinearProgress
            variant="determinate"
            value={value ?? 0}
            color="secondary"
            style={{ height: '25px', width: '100%', textAlign: 'center' }}
          />
          <div style={{ position: 'absolute', top: '3px', textAlign: 'left', paddingLeft: '10px', width: '100%' }}>
            <Typography variant="body2" color="textSecondary" component="div">
              {uniqueCardsCollected}/{totalCards}
            </Typography>
          </div>
          <div style={{ position: 'absolute', top: '3px', textAlign: 'right', paddingRight: '10px', width: '100%' }}>
            <Typography variant="body2" color="textSecondary" component="div">
              {value}%
            </Typography>
          </div>
        </div>
      );
    },
  },
  {
    accessor: `costsToPurchaseInSet[${priceType}].totalValue`,
    Header: 'Current Value',
    Cell: ({ cell: { value } }) => formatter.format(value),
  },
  {
    accessor: 'costsToPurchaseInSet.uniquePrintingsCollectedInSet',
    Header: 'Unique Cards Collected',
  },
  {
    accessor: 'costsToPurchaseInSet.totalCardsCollectedInSet',
    Header: 'Total Cards Collected',
  },
  {
    accessor: 'cardCount',
    Header: 'Cards In Set',
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
    Cell: ({ cell: { value } }) => value?.slice(0, 10) ?? null,
  },
  {
    accessor: 'slug',
    Header: 'Slug',
  },
];

export const browseTableColumns = (priceType) => [
  {
    accessor: 'id',
    Header: 'MTG CB ID',
  },
  {
    accessor: 'code',
    Header: 'Code',
  },
  {
    accessor: 'name',
    Header: 'Name',
    Cell: (cell) => {
      const setName = cell?.row?.values?.name ?? 'Unknown Set';
      const setSlug = cell?.row?.values?.slug ?? 'unknown-set';
      return (
        <Link href={`/browse/sets/${setSlug}`} variant="body2">
          {setName}
        </Link>
      );
    },
    sortType: (a, b) => (a?.values?.set?.name ?? '').localeCompare(b?.values?.set?.name ?? ''),
  },
  {
    accessor: 'cardCount',
    Header: 'Cards In Set',
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
    Cell: ({ cell: { value } }) => value?.slice(0, 10) ?? null,
  },
  {
    accessor: 'slug',
    Header: 'Slug',
  },
];
