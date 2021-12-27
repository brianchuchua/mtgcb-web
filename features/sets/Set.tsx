import Breadcrumbs from '@material-ui/core/Breadcrumbs';
import Container from '@material-ui/core/Container';
import Typography from '@material-ui/core/Typography';
import { memo, useEffect, useState } from 'react';
import { useDispatch, useSelector } from 'react-redux';
import Link from '../../components/Link';
import { useGetAllCardsMetaQuery, useGetAllCardsQuery, useGetSetBySlugQuery } from '../../network/services/mtgcbApi';
import { RootState } from '../../redux/rootReducer';
import useDebounce, { searchFieldDebounceTimeMs } from '../../util/useDebounce';
import CardGallery from '../browse/CardGallery';
import CardTable from '../browse/CardTable';
import titleCase from '../browse/util/titleCase';
import { setFormVisibility } from './setSlice';

interface SetProps {
  setSlug: string;
}

export const Set: React.FC<SetProps> = ({ setSlug }) => {
  const {
    searchQuery,
    oracleTextQuery,
    cardTypes,
    cardRarities,
    cardColors,
    showAllPrintings,
    cardStatSearches,
    sortBy,
    sortByDirection,
    viewSubject,
    viewMode,
    priceType,
  } = useSelector((state: RootState) => state.set);

  const dispatch = useDispatch();

  useEffect(() => {
    dispatch(setFormVisibility({ isFormVisibile: true }));
    return function cleanUpForm() {
      dispatch(setFormVisibility({ isFormVisibile: false }));
    };
  }, []);

  const debouncedSearchQuery = useDebounce(searchQuery, searchFieldDebounceTimeMs);
  const debouncedOracleTextQuery = useDebounce(oracleTextQuery, searchFieldDebounceTimeMs);

  const [skip, setSkip] = useState(0);
  const [first, setFirst] = useState(50);
  const [page, setPage] = useState(1);

  const { data: setData, isLoading: isSetLoading, error: setError } = useGetSetBySlugQuery({ slug: setSlug });

  const { data: cardData, isLoading: isCardDataLoading, error: cardError } = useGetAllCardsQuery({
    first,
    skip,
    sortBy,
    name: debouncedSearchQuery,
    oracleTextQuery: debouncedOracleTextQuery,
    cardSets: [
      {
        category: 'Sets',
        value: setData?.data?.allSets?.[0]?.id,
        label: setData?.data?.allSets?.[0]?.name,
        exclude: false,
      },
    ],
    cardRarities,
    cardTypes,
    cardColors,
    showAllPrintings,
    cardStatSearches,
    sortByDirection,
  });

  const { data: cardMetaData, isLoading: isCardMetaDataLoading, error: cardMetaError } = useGetAllCardsMetaQuery({
    sortBy,
    name: debouncedSearchQuery,
    oracleTextQuery: debouncedOracleTextQuery,
    cardSets: [
      {
        category: 'Sets',
        value: setData?.data?.allSets?.[0]?.id,
        label: setData?.data?.allSets?.[0]?.name,
        exclude: false,
      },
    ],
    cardRarities,
    cardTypes,
    cardColors,
    showAllPrintings,
    cardStatSearches,
    sortByDirection,
  });

  const set = setData?.data?.allSets?.[0];
  const cards = cardData?.data?.allCards;
  const totalResults = cardMetaData?.data?._allCardsMeta?.count;

  // TODO: Make a nice set icon component with intelligent fallbacks or a default option
  // TODO: Add buy links here and come up with a good interface, similar to how Scryfall does card pages perhaps
  // TODO: Add charts/analysis/something cool here

  // TODO: Make better Breadcrumbs component
  return (
    <Container maxWidth="xl">
      <Breadcrumbs separator=">" aria-label="breadcrumb">
        <Link href="/" variant="body2" color="inherit">
          MTG CB
        </Link>
        <Link href="/browse" variant="body2" color="inherit">
          Browse
        </Link>
        <Link href="/browse" variant="body2" color="inherit">
          Sets
        </Link>
        <Link href={`/browse/sets/${set?.slug}`} variant="body2" color="inherit">
          {set?.name ?? 'Unknown Set'}
        </Link>
      </Breadcrumbs>
      <div>
        {set ? (
          <div style={{ textAlign: 'center' }}>
            <Typography variant="h4" component="div">
              {set?.name}
            </Typography>
            <div style={{ textAlign: 'center' }}>
              <i
                className={`ss ss-${set.code.toLowerCase()} ss-5x ss-common ss-fw`}
                style={{
                  // WebkitTextStroke: '1px #fff', // TODO: Use this style for a complete set so I can support ss-mythic ss-grad
                  textShadow: '-1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff',
                  paddingBottom: '5px',
                }}
              />
              <Typography variant="body2" color="textSecondary" component="div">
                {set.releasedAt?.slice(0, 10)}
              </Typography>
              <Typography variant="body2" color="textSecondary" component="div">
                {set.cardCount ? `${set.cardCount} cards` : ''}
              </Typography>
              <Typography variant="body2" color="textSecondary" component="div">
                {set.category} Set
                {set.setType ? ` - ${titleCase(set.setType)}` : ''}
              </Typography>
            </div>
            {viewSubject === 'cards' && viewMode === 'grid' && (
              <MemoizedCardGallery
                cards={cards}
                totalResults={totalResults}
                first={first}
                skip={skip}
                page={page}
                setSkip={setSkip}
                setFirst={setFirst}
                setPage={setPage}
                priceType={priceType}
              />
            )}
            {viewSubject === 'cards' && viewMode === 'table' && (
              <MemoizedCardTable
                cards={cards}
                totalResults={totalResults}
                first={first}
                skip={skip}
                page={page}
                setSkip={setSkip}
                setFirst={setFirst}
                setPage={setPage}
                priceType={priceType}
                isShowingSingleSet
              />
            )}
          </div>
        ) : (
          <p>No set found</p>
        )}
      </div>
    </Container>
  );
};

const MemoizedCardGallery = memo(CardGallery);
const MemoizedCardTable = memo(CardTable);
