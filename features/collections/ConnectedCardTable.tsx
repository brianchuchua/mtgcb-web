import React, { memo, useMemo } from 'react';
import { useSelector } from 'react-redux';
import { useGetAllCardsMetaQuery, useGetAllCardsQuery, useGetCollectionByCardIdLegacyQuery } from '../../network/services/mtgcbApi';
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

  const cards = cardData?.data?.allCards;
  const cardIds = cards?.map((card) => card.id);
  const totalResults = cardMetaData?.data?._allCardsMeta?.count;

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

  const collectionByCardId = useMemo(
    () =>
      collectionByCardIdResponse?.data?.collectionByCardIdLegacy?.collection?.reduce((acc, curr) => {
        acc[curr.cardID] = curr;
        return acc;
      }, {} as any),
    [collectionByCardIdResponse]
  ); // eslint-disable-line @typescript-eslint/no-explicit-any

  return (
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

const MemoizedCardTable = memo(CardTable, tableShouldNotRerender);
