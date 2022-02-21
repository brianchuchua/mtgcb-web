import LinearProgress from '@material-ui/core/LinearProgress';
import Typography from '@material-ui/core/Typography';
import { memo, useEffect, useState } from 'react';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';
import { ResponsiveContainer } from '../../components/layout/ResponsiveContainer';
import {
  useGetAllCardsMetaQuery,
  useGetAllCardsQuery,
  useGetAllSetsMetaQuery,
  useGetAllSetsQuery,
  useGetCollectionByCardIdLegacyQuery,
  useGetCollectionSummaryLegacyQuery,
} from '../../network/services/mtgcbApi';
import { RootState } from '../../redux/rootReducer';
import useDebounce, { searchFieldDebounceTimeMs } from '../../util/useDebounce';
import CardGallery from '../browse/CardGallery';
import CardTable from '../browse/CardTable';
import SetGallery from '../browse/SetGallery';
import SetTable from '../browse/SetTable';
import { formatter } from '../browse/util/formatPrice';
import { setFormVisibility } from './collectionSlice';

export const Collection: React.FC<CollectionProps> = ({ userId }) => {
  const {
    searchQuery,
    oracleTextQuery,
    cardTypes,
    cardSets,
    cardRarities,
    cardColors,
    showAllPrintings,
    cardStatSearches,
    sortBy,
    sortByDirection,
    viewSubject,
    viewMode,
    priceType,
    expansionSearchQuery,
    sortExpansionBy,
    sortExpansionByDirection,
    expansionTypes,
    expansionCategories,
  } = useSelector((state: RootState) => state.collection);

  const debouncedSearchQuery = useDebounce(searchQuery, searchFieldDebounceTimeMs);
  const debouncedOracleTextQuery = useDebounce(oracleTextQuery, searchFieldDebounceTimeMs);
  const debouncedExpansionSearchQuery = useDebounce(expansionSearchQuery, searchFieldDebounceTimeMs);

  const [skip, setSkip] = useState(0);
  const [first, setFirst] = useState(50);
  const [page, setPage] = useState(1);

  const [expansionsSkip, setExpansionsSkip] = useState(0);
  const [expansionsFirst, setExpansionsFirst] = useState(16);
  const [expansionsPage, setExpansionsPage] = useState(1);

  const dispatch = useDispatch();

  useEffect(() => {
    dispatch(setFormVisibility({ isFormVisibile: true }));
    return function cleanUpForm() {
      dispatch(setFormVisibility({ isFormVisibile: false }));
    };
  }, []);

  const { data: cardData, isLoading: isCardDataLoading, error: cardError } = useGetAllCardsQuery({
    first,
    skip,
    sortBy,
    name: debouncedSearchQuery,
    oracleTextQuery: debouncedOracleTextQuery,
    cardSets,
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
    cardSets,
    cardRarities,
    cardTypes,
    cardColors,
    showAllPrintings,
    cardStatSearches,
    sortByDirection,
  });

  const {
    data: collectionSummary,
    isLoading: isCollectionSummaryLoading,
    error: collectionSummaryError,
  } = useGetCollectionSummaryLegacyQuery({ userId });

  const costsToPurchase = collectionSummary?.data?.collectionSummaryLegacy?.collectionSummary;
  const username = collectionSummary?.data?.collectionSummaryLegacy?.username;
  const collectionDetails = {
    username,
    numberOfCardsInMagic: collectionSummary?.data?.collectionSummaryLegacy?.numberOfCardsInMagic,
    totalCardsCollected: collectionSummary?.data?.collectionSummaryLegacy?.totalCardsCollected,
    uniquePrintingsCollected: collectionSummary?.data?.collectionSummaryLegacy?.uniquePrintingsCollected,
    percentageCollected: collectionSummary?.data?.collectionSummaryLegacy?.percentageCollected,
    totalValue: collectionSummary?.data?.collectionSummaryLegacy?.totalValue?.[priceType],
  };

  const cards = cardData?.data?.allCards;
  const totalResults = cardMetaData?.data?._allCardsMeta?.count;

  useEffect(() => {
    if (skip > totalResults) {
      setSkip(0);
      setPage(1);
    }
  }, [skip, totalResults]);

  const cardIds = cards?.map((card) => card.id);

  const {
    data: collectionByCardIdResponse,
    isLoading: isCollectionByCardIdLoading,
    error: collectionByCardIdError,
  } = useGetCollectionByCardIdLegacyQuery(
    {
      userId,
      cardIds,
    },
    { skip: cardIds == null }
  );

  const collectionByCardId = collectionByCardIdResponse?.data?.collectionByCardIdLegacy?.collection?.reduce((acc, curr) => {
    acc[curr.cardID] = curr;
    return acc;
  }, {} as any); // eslint-disable-line @typescript-eslint/no-explicit-any

  const { data: allSetsResponse } = useGetAllSetsQuery({
    first: expansionsFirst,
    skip: expansionsSkip,
    name: debouncedExpansionSearchQuery,
    sortBy: sortExpansionBy,
    sortByDirection: sortExpansionByDirection,
    setTypes: expansionTypes,
    setCategories: expansionCategories,
  });
  const expansions = allSetsResponse?.data?.allSets;

  const { data: allSetsMetaResponse } = useGetAllSetsMetaQuery({
    name: debouncedExpansionSearchQuery,
    sortBy: sortExpansionBy,
    sortByDirection: sortExpansionByDirection,
    setTypes: expansionTypes,
    setCategories: expansionCategories,
  });
  const allSetsMeta = allSetsMetaResponse?.data?._allSetsMeta;
  const totalExpansionsResults = allSetsMeta?.count || 0;

  useEffect(() => {
    if (expansionsSkip > totalExpansionsResults) {
      setExpansionsSkip(0);
      setExpansionsPage(1);
    }
  }, [expansionsSkip, totalExpansionsResults]);

  return (
    <ResponsiveContainer maxWidth="xl">
      {collectionDetails && <CollectionDetails collectionDetails={collectionDetails} />}
      <ContentWrapper>
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
            userId={userId}
            collectionByCardId={collectionByCardId}
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
            userId={userId}
            collectionByCardId={collectionByCardId}
          />
        )}
        {viewSubject === 'sets' && viewMode === 'grid' && (
          <MemoizedSetGallery
            sets={expansions}
            costsToPurchase={costsToPurchase}
            totalResults={totalExpansionsResults}
            first={expansionsFirst}
            skip={expansionsSkip}
            page={expansionsPage}
            setSkip={setExpansionsSkip}
            setFirst={setExpansionsFirst}
            setPage={setExpansionsPage}
            priceType={priceType}
            userId={userId}
          />
        )}
        {viewSubject === 'sets' && viewMode === 'table' && (
          <MemoizedSetTable
            sets={expansions}
            costsToPurchase={costsToPurchase}
            totalResults={totalExpansionsResults}
            first={expansionsFirst}
            skip={expansionsSkip}
            page={expansionsPage}
            setSkip={setExpansionsSkip}
            setFirst={setExpansionsFirst}
            setPage={setExpansionsPage}
            priceType={priceType}
            isCollectorMode
            userId={userId}
          />
        )}
      </ContentWrapper>
    </ResponsiveContainer>
  );
};

const tableShouldNotRerender = (prevProps, nextProps) => {
  if (prevProps?.cards?.length !== nextProps?.cards?.length) {
    return false;
  }
  const idsAreTheSame = prevProps?.cards?.map((card) => card.id).join(',') === nextProps?.cards?.map((card) => card.id).join(',');
  if (idsAreTheSame) {
    return true;
  }
  return false;
};

const MemoizedSetGallery = memo(SetGallery);
const MemoizedCardGallery = memo(CardGallery);
const MemoizedCardTable = memo(CardTable, tableShouldNotRerender);
const MemoizedSetTable = memo(SetTable);

const ContentWrapper = styled.div(({ theme }) => ({
  marginTop: theme.spacing(0),
}));

interface CollectionProps {
  userId: string;
}

interface CollectionDetailsProps {
  collectionDetails: {
    username: string;
    numberOfCardsInMagic: number;
    totalCardsCollected: number;
    uniquePrintingsCollected: number;
    percentageCollected: number;
    totalValue: number;
  };
}

const CollectionDetails: React.FC<CollectionDetailsProps> = ({ collectionDetails }) => (
  <CollectionDetailsWrapper>
    <StyledCollectionDetailsTitleDesktop variant="h3">{collectionDetails.username}'s Collection</StyledCollectionDetailsTitleDesktop>
    <StyledCollectionDetailsTitleMobile variant="h5">{collectionDetails.username}'s Collection</StyledCollectionDetailsTitleMobile>
    <CollectionDetailsBody>
      <StyledCollectionDetailsTitleDesktop variant="h4" color="textSecondary">
        {collectionDetails.uniquePrintingsCollected}/{collectionDetails.numberOfCardsInMagic}
      </StyledCollectionDetailsTitleDesktop>
      <StyledCollectionDetailsTitleMobile variant="h6" color="textSecondary">
        {collectionDetails.uniquePrintingsCollected}/{collectionDetails.numberOfCardsInMagic}
      </StyledCollectionDetailsTitleMobile>
      <Typography variant="body2" color="textSecondary" component="div">
        <em>({collectionDetails.totalCardsCollected} total cards collected)</em>
      </Typography>
      <Typography variant="body2" color="textSecondary" component="div">
        <em>Collection value: {formatter.format(collectionDetails.totalValue)}</em>
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

const StyledCollectionDetailsTitleMobile = styled(CollectionDetailsTitle)(({ theme }) => ({
  [theme.breakpoints.up('lg')]: {
    display: 'none',
  },
}));

const StyledCollectionDetailsTitleDesktop = styled(CollectionDetailsTitle)(({ theme }) => ({
  [theme.breakpoints.down('md')]: {
    display: 'none',
  },
}));

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
