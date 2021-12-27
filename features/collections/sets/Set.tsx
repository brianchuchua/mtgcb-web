import Breadcrumbs from '@material-ui/core/Breadcrumbs';
import Container from '@material-ui/core/Container';
import LinearProgress from '@material-ui/core/LinearProgress';
import Typography from '@material-ui/core/Typography';
import { memo, useEffect, useState } from 'react';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';
import Link from '../../../components/Link';
import {
  useGetAllCardsMetaQuery,
  useGetAllCardsQuery,
  useGetSetBySlugQuery,
  useGetSetSummaryLegacyQuery,
} from '../../../network/services/mtgcbApi';
import { RootState } from '../../../redux/rootReducer';
import useDebounce, { searchFieldDebounceTimeMs } from '../../../util/useDebounce';
import CardGallery from '../../browse/CardGallery';
import CardTable from '../../browse/CardTable';
import { SetIcon } from '../../browse/SetBox';
import { formatter } from '../../browse/util/formatPrice';
import { setFormVisibility } from './setCollectionSlice';

interface SetProps {
  setSlug: string;
  userId?: string;
}

export const Set: React.FC<SetProps> = ({ setSlug, userId }) => {
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
  } = useSelector((state: RootState) => state.setCollection);

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

  const setId = setData?.data?.allSets?.[0]?.id;
  const { data: setSummaryData, isLoading: isSetSummaryLoading, error: setSummaryError } = useGetSetSummaryLegacyQuery(
    {
      setId: setData?.data?.allSets?.[0]?.id,
      userId,
    },
    { skip: setId == null }
  );

  const set = setData?.data?.allSets?.[0];
  const cards = cardData?.data?.allCards;
  const totalResults = cardMetaData?.data?._allCardsMeta?.count;
  const setSummary = setSummaryData?.data?.setSummaryLegacy;
  const username = setSummary?.username ?? '';
  const collection = setSummary?.collection;

  const collectionByCardId = collection?.reduce((acc, curr) => {
    acc[curr.cardID] = curr;
    return acc;
  }, {} as any); // eslint-disable-line @typescript-eslint/no-explicit-any

  console.log(setSummary); // TODO: It's a defaulting issue

  const collectionDetails = {
    setName: set?.name,
    setCode: set?.code,
    username,
    userId,
    cardsInSet: setSummary?.cardsInSet,
    totalCardsCollectedInSet: setSummary?.totalCardsCollectedInSet,
    uniquePrintingsCollectedInSet: setSummary?.uniquePrintingsCollectedInSet,
    percentageCollected: setSummary?.percentageCollected,
    totalValue: setSummary?.totalValue, // TODO: Implement, probably needs to be added to the API
  };

  // TODO: Add buy links here and come up with a good interface, similar to how Scryfall does card pages perhaps

  // TODO: Make better Breadcrumbs component
  return (
    <Container maxWidth="xl">
      <Breadcrumbs separator=">" aria-label="breadcrumb">
        <Link href="/" variant="body2" color="inherit">
          MTG CB
        </Link>
        <Link href={`/collections/${userId}`} variant="body2" color="inherit">
          Collections
        </Link>
        <Link href={`/collections/${userId}`} variant="body2" color="inherit">
          {username}'s Collection
        </Link>
        <Link href={`/collections/${userId}`} variant="body2" color="inherit">
          Sets
        </Link>
        <Link href={`/collections/${userId}/sets/${set?.slug}`} variant="body2" color="inherit">
          {set?.name ?? 'Unknown Set'}
        </Link>
      </Breadcrumbs>
      <>
        {set ? (
          <>
            <SetCollectionDetails collectionDetails={collectionDetails} priceType={priceType} />
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
                collectionByCardId={collectionByCardId}
                userId={userId}
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
                collectionByCardId={collectionByCardId}
                userId={userId}
                isShowingSingleSet
              />
            )}
          </>
        ) : (
          <p>No set found</p>
        )}
      </>
    </Container>
  );
};

const MemoizedCardGallery = memo(CardGallery);
const MemoizedCardTable = memo(CardTable);

interface SetCollectionDetails {
  collectionDetails: {
    setName: string;
    setCode: string;
    username: string;
    userId: string;
    cardsInSet: number;
    totalCardsCollectedInSet: number;
    uniquePrintingsCollectedInSet: number;
    percentageCollected: number;
    totalValue: {
      market: number;
      low: number;
      average: number;
      high: number;
    };
  };
  priceType: string;
}

const SetCollectionDetails: React.FC<SetCollectionDetails> = ({ collectionDetails, priceType }) => (
  <CollectionDetailsWrapper>
    <CollectionDetailsTitle variant="h4">{collectionDetails.setName}</CollectionDetailsTitle>
    <CollectionDetailsSubtitle variant="body1" color="textSecondary">
      <em>
        <Link href={`/collections/${collectionDetails.userId}`} color="inherit">
          (Part of {collectionDetails.username}'s collection)
        </Link>
      </em>
    </CollectionDetailsSubtitle>
    <CollectionDetailsBody>
      <SetIcon setCode={collectionDetails.setCode} />
      <Typography variant="h5" color="textSecondary" component="div">
        {collectionDetails.uniquePrintingsCollectedInSet}/{collectionDetails.cardsInSet}
      </Typography>
      <Typography variant="body2" color="textSecondary" component="div">
        <em>({collectionDetails.totalCardsCollectedInSet} total cards collected)</em>
      </Typography>
      <Typography variant="body2" color="textSecondary" component="div">
        <em>Set value: {collectionDetails?.totalValue && formatter.format(collectionDetails?.totalValue?.[priceType])}</em>
      </Typography>
      <CollectionProgressWrapper>
        <StyledLinearProgress variant="determinate" value={collectionDetails.percentageCollected ?? 0} color="secondary" />
        <LinearProgressLabel>
          <Typography variant="body2" color="textSecondary" component="div">
            {collectionDetails.percentageCollected}% collected!
          </Typography>
        </LinearProgressLabel>
      </CollectionProgressWrapper>
    </CollectionDetailsBody>
  </CollectionDetailsWrapper>
);

const CollectionDetailsWrapper = styled.div(() => ({
  width: '100%',
}));

const CollectionDetailsTitle = styled(Typography)({
  textAlign: 'center',
});

const CollectionDetailsSubtitle = styled(Typography)({
  textAlign: 'center',
});

const CollectionDetailsBody = styled.div({
  textAlign: 'center',
});

const CollectionProgressWrapper = styled.div({
  position: 'relative',
});

const StyledLinearProgress = styled(LinearProgress)({
  height: '25px',
  width: '100%',
  margin: '10px auto 10px',
  textAlign: 'center',
});

const LinearProgressLabel = styled.div({
  position: 'absolute',
  top: '3px',
  textAlign: 'center',
  margin: '0 auto',
  width: '100%',
});
