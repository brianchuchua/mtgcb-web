import React, { memo, useCallback, useEffect, useMemo } from 'react';
import { useSelector } from 'react-redux';
import {
  useGetAllCardsMetaQuery,
  useGetAllCardsQuery,
  useGetCollectionByCardIdLegacyQuery,
  usePrefetch,
} from '../../network/services/mtgcbApi';
import { RootState } from '../../redux/rootReducer';
import useDebounce, { searchFieldDebounceTimeMs } from '../../util/useDebounce';
import CardGallery from '../browse/CardGallery';

interface ConnectedCardGalleryProps {
  userId: string;
  first: number;
  skip: number;
  page: number;
  setSkip: (skip: number) => void;
  setFirst: (first: number) => void;
  setPage: (page: number) => void;
}

export const ConnectedCardGallery: React.FC<ConnectedCardGalleryProps> = ({ userId, first, skip, page, setSkip, setFirst, setPage }) => {
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

  const { data: cardData, isLoading: isCardDataLoading, isFetching: isCardDataFetching, error: cardError } = useGetAllCardsQuery({
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

  const {
    data: cardMetaData,
    isLoading: isCardMetaDataLoading,
    isFetching: isCardMetaDataFetching,
    error: cardMetaError,
  } = useGetAllCardsMetaQuery({
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
    { skip: cardIds == null }
  );

  const collectionByCardId = useMemo(
    () =>
      collectionByCardIdResponse?.data?.collectionByCardIdLegacy?.collection?.reduce((acc, curr) => {
        acc[curr.cardID] = curr;
        return acc;
      }, {} as any),
    [collectionByCardIdResponse]
  ); // eslint-disable-line @typescript-eslint/no-explicit-any

  const isLoading = isCardDataLoading || isCardMetaDataLoading || isCollectionByCardIdLoading;
  const isFetching = isCardDataFetching || isCardMetaDataFetching || isCollectionByCardIdFetching;

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
      isLoading={isLoading}
      isFetching={isFetching}
    />
  );
};

const MemoizedCardGallery = memo(CardGallery);
