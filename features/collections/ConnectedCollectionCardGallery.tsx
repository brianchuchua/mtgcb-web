import React, { memo, useEffect, useMemo, useState } from 'react';
import { useSelector } from 'react-redux';
import { useGetFilteredCardsSummaryLegacyQuery } from '../../network/services/mtgcbApi';
import { RootState } from '../../redux/rootReducer';
import useDebounce, { searchFieldDebounceTimeMs } from '../../util/useDebounce';
import CardGallery from '../browse/CardGallery';

interface ConnectedConnectionCardGalleryProps {
  userId: string;
  setId: string;
  first: number;
  skip: number;
  page: number;
  setSkip: (skip: number) => void;
  setFirst: (first: number) => void;
  setPage: (page: number) => void;
}

export const ConnectedCollectionCardGallery: React.FC<ConnectedConnectionCardGalleryProps> = ({
  userId,
  setId,
  first,
  skip,
  page,
  setSkip,
  setFirst,
  setPage,
}) => {
  const {
    sortBy,
    sortByDirection,
    priceType,
    searchQuery,
    oracleTextQuery,
    cardRarities,
    cardTypes,
    cardColors,
    showAllPrintings,
    cardStatSearches,
  } = useSelector((state: RootState) => state.setCollection);

  const debouncedSearchQuery = useDebounce(searchQuery, searchFieldDebounceTimeMs);
  const debouncedOracleTextQuery = useDebounce(oracleTextQuery, searchFieldDebounceTimeMs);
  const [previousTotalResults, setPreviousTotalResults] = useState(null);

  const {
    data: filteredCardsSummary,
    isLoading: loadingFilteredCardsSummary,
    isFetching: fetchingFilteredCardsSummary,
  } = useGetFilteredCardsSummaryLegacyQuery({
    userId,
    setId,
    first,
    skip,
    sortBy,
    sortByDirection,
    name: debouncedSearchQuery,
    oracleTextQuery: debouncedOracleTextQuery,
    cardSets: [
      {
        category: 'Sets',
        value: setId,
        label: setId,
        exclude: false,
      },
    ],
    cardRarities,
    cardTypes,
    cardColors,
    showAllPrintings,
    cardStatSearches,
    additionalWhere: null,
    additionalSortBy: null,
  });

  const cards = filteredCardsSummary?.data?.filteredCardsSummaryLegacy?.cards;
  const totalResults = filteredCardsSummary?.data?.filteredCardsSummaryLegacy?.count;

  useEffect(() => {
    if (totalResults !== previousTotalResults) {
      setSkip(0);
      setPage(1);
      setPreviousTotalResults(totalResults);
    }
    if (skip > totalResults) {
      setSkip(0);
      setPage(1);
    }
  }, [skip, totalResults, previousTotalResults]);

  const collectionByCardId = useMemo(
    () =>
      filteredCardsSummary?.data?.filteredCardsSummaryLegacy?.cards?.reduce((acc, curr) => {
        acc[curr.id] = curr;
        return acc;
      }, {} as any),
    [filteredCardsSummary?.data?.filteredCardsSummaryLegacy?.cards]
  ); // eslint-disable-line @typescript-eslint/no-explicit-any

  return (
    <MemoizedCollectionCardGallery
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
      isLoading={loadingFilteredCardsSummary}
      isFetching={fetchingFilteredCardsSummary}
    />
  );
};

const MemoizedCollectionCardGallery = memo(CardGallery);
