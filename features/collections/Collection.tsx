import Breadcrumbs from '@material-ui/core/Breadcrumbs';
import Container from '@material-ui/core/Container';
import { memo, useEffect, useState } from 'react';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';
import Link from '../../components/Link';
import {
  useGetAllCardsMetaQuery,
  useGetAllCardsQuery,
  useGetAllSetsMetaQuery,
  useGetAllSetsQuery,
  useGetCollectionSummaryLegacyQuery,
} from '../../network/services/mtgcbApi';
import { RootState } from '../../redux/rootReducer';
import useDebounce, { searchFieldDebounceTimeMs } from '../../util/useDebounce';
import CardGallery from '../browse/CardGallery';
import CardTable from '../browse/CardTable';
import SetGallery from '../browse/SetGallery';
import SetTable from '../browse/SetTable';
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
  const [expansionsFirst, setExpansionsFirst] = useState(50);
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

  const cards = cardData?.data?.allCards;
  const totalResults = cardMetaData?.data?._allCardsMeta?.count;

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

  // TODO: Make better Breadcrumbs component
  return (
    <Container maxWidth="xl">
      <Breadcrumbs separator=">" aria-label="breadcrumb">
        <Link href="/" variant="body2" color="inherit">
          MTG CB
        </Link>
        <Link href="#" variant="body2" color="inherit">
          Collections
        </Link>
        <Link href={`/collections/${userId}`} variant="body2" color="inherit">
          {username}'s Collection
        </Link>
      </Breadcrumbs>
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
            totalResults={totalExpansionsResults}
            first={expansionsFirst}
            skip={expansionsSkip}
            page={expansionsPage}
            setSkip={setExpansionsSkip}
            setFirst={setExpansionsFirst}
            setPage={setExpansionsPage}
            priceType={priceType}
          />
        )}
      </ContentWrapper>
    </Container>
  );
};

const MemoizedSetGallery = memo(SetGallery);
const MemoizedCardGallery = memo(CardGallery);
const MemoizedCardTable = memo(CardTable);
const MemoizedSetTable = memo(SetTable);

const ContentWrapper = styled.div(({ theme }) => ({
  marginTop: theme.spacing(0),
}));

interface CollectionProps {
  userId: string;
}
