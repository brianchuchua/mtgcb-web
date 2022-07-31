import React, { memo, useCallback, useEffect, useMemo } from 'react';
import { useSelector } from 'react-redux';
import {
  useGetAllCardsMetaQuery,
  useGetAllCardsQuery,
  useGetCollectionByCardIdLegacyQuery,
  useGetFilteredCardsSummaryLegacyQuery,
  usePrefetch,
} from '../../network/services/mtgcbApi';
import { RootState } from '../../redux/rootReducer';
import useDebounce, { searchFieldDebounceTimeMs } from '../../util/useDebounce';
import CardTable from '../browse/CardTable';

interface ConnectedCardTableProps {
  userId: string;
  first: number;
  skip: number;
  page: number;
  setSkip: (skip: number) => void;
  setFirst: (first: number) => void;
  setPage: (page: number) => void;
}

export const ConnectedCardTable: React.FC<ConnectedCardTableProps> = ({ userId, first, skip, page, setSkip, setFirst, setPage }) => {
  const {
    sortBy,
    sortByDirection,
    priceType,
    searchQuery,
    oracleTextQuery,
    cardSets,
    cardRarities,
    cardTypes,
    cardColors,
    showAllPrintings,
    cardStatSearches,
  } = useSelector((state: RootState) => state.collection);

  const debouncedSearchQuery = useDebounce(searchQuery, searchFieldDebounceTimeMs);
  const debouncedOracleTextQuery = useDebounce(oracleTextQuery, searchFieldDebounceTimeMs);

  // This deserves a custom hook, but this is used to determine which sets of APIs to use for the card gallery.
  // If a user is trying to search or sort by quantity, we have to involve an external legacy database and do a manual remote join
  const includesQuantityFilters = useMemo(() => {
    for (const cardStatSearch of cardStatSearches) {
      const { searchAttribute, value } = cardStatSearch;
      if (value !== '' && (searchAttribute === 'cardsAll' || searchAttribute === 'cardsNormal' || searchAttribute === 'cardsFoil')) {
        return true;
      }
    }
    if (
      sortBy === 'currentValue' ||
      sortBy === 'costToComplete' ||
      sortBy === 'percentageCollected' ||
      sortBy === 'quantityAll' ||
      sortBy === 'quantityNormal' ||
      sortBy === 'quantityFoil'
    ) {
      return true;
    }

    return false;
  }, [cardStatSearches, sortBy]);

  const { data: cardData, isLoading: isCardDataLoading, isFetching: isCardDataFetching, error: cardError } = useGetAllCardsQuery(
    {
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
    },
    { skip: includesQuantityFilters }
  );

  const {
    data: cardMetaData,
    isLoading: isCardMetaDataLoading,
    isFetching: isCardMetaDataFetching,
    error: cardMetaError,
  } = useGetAllCardsMetaQuery(
    {
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
    },
    { skip: includesQuantityFilters }
  );

  const cards = cardData?.data?.allCards;
  const cardIds = cards?.map((card) => card.id);
  const totalResults = cardMetaData?.data?._allCardsMeta?.count;

  const {
    data: collectionByCardIdResponse,
    isLoading: isCollectionByCardIdLoading,
    isFetching: isCollectionByCardIdFetching,
    error: collectionByCardIdError,
  } = useGetCollectionByCardIdLegacyQuery(
    {
      userId,
      cardIds,
    },
    { skip: cardIds == null || includesQuantityFilters }
  );

  const collectionByCardId = useMemo(
    () =>
      collectionByCardIdResponse?.data?.collectionByCardIdLegacy?.collection?.reduce((acc, curr) => {
        acc[curr.cardID] = curr;
        return acc;
      }, {} as any),
    [collectionByCardIdResponse]
  ); // eslint-disable-line @typescript-eslint/no-explicit-any

  const {
    data: filteredCardsSummary,
    isLoading: loadingFilteredCardsSummary,
    isFetching: fetchingFilteredCardsSummary,
  } = useGetFilteredCardsSummaryLegacyQuery(
    {
      userId,
      first,
      skip,
      sortBy,
      sortByDirection,
      name: debouncedSearchQuery,
      oracleTextQuery: debouncedOracleTextQuery,
      cardRarities,
      cardTypes,
      cardColors,
      showAllPrintings,
      cardStatSearches,
      additionalWhere: null,
      additionalSortBy: null,
    },
    { skip: !includesQuantityFilters }
  );

  const cardsFromHeavyQuery = filteredCardsSummary?.data?.filteredCardsSummaryLegacy?.cards;
  const totalResultsFromHeavyQuery = filteredCardsSummary?.data?.filteredCardsSummaryLegacy?.count;
  const collectionByCardIdFromHeavyQuery = useMemo(
    () =>
      filteredCardsSummary?.data?.filteredCardsSummaryLegacy?.cards?.reduce((acc, curr) => {
        acc[curr.id] = curr;
        return acc;
      }, {} as any),
    [filteredCardsSummary?.data?.filteredCardsSummaryLegacy?.cards]
  ); // eslint-disable-line @typescript-eslint/no-explicit-any

  const isLoading = isCardDataLoading || isCardMetaDataLoading || isCollectionByCardIdLoading || loadingFilteredCardsSummary;
  const isFetching = isCardDataFetching || isCardMetaDataFetching || fetchingFilteredCardsSummary;

  const prefetchAllCards = usePrefetch('getAllCards');

  const prefetchNextAllCards = useCallback(() => {
    if (skip + first < totalResults) {
      prefetchAllCards({
        first,
        skip: skip + first,
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
    }
  }, [
    prefetchAllCards,
    skip,
    first,
    totalResults,
    sortBy,
    debouncedSearchQuery,
    debouncedOracleTextQuery,
    cardSets,
    cardRarities,
    cardTypes,
    cardColors,
    showAllPrintings,
    cardStatSearches,
    sortByDirection,
  ]);

  useEffect(() => {
    if (skip + first < totalResults) {
      prefetchNextAllCards();
    }
  }, [skip, first, totalResults, prefetchNextAllCards]);

  return (
    <MemoizedCardTable
      cards={includesQuantityFilters ? cardsFromHeavyQuery : cards}
      totalResults={includesQuantityFilters ? totalResultsFromHeavyQuery : totalResults}
      first={first}
      skip={skip}
      page={page}
      setSkip={setSkip}
      setFirst={setFirst}
      setPage={setPage}
      priceType={priceType}
      userId={userId}
      collectionByCardId={includesQuantityFilters ? collectionByCardIdFromHeavyQuery : collectionByCardId}
      isFetching={isFetching}
    />
  );
};

const tableShouldNotRerender = (prevProps, nextProps) => {
  if (prevProps?.cards?.length !== nextProps?.cards?.length || prevProps.isFetching !== nextProps.isFetching) {
    return false;
  }
  const idsAreTheSame = prevProps?.cards?.map((card) => card.id).join(',') === nextProps?.cards?.map((card) => card.id).join(',');
  if (idsAreTheSame) {
    return true;
  }
  return false;
};

const MemoizedCardTable = memo(CardTable, tableShouldNotRerender);
