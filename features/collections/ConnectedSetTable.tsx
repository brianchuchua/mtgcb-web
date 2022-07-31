import React, { memo } from 'react';
import { useSelector } from 'react-redux';
import { useGetFilteredCollectionSummaryLegacyQuery } from '../../network/services/mtgcbApi';
import { RootState } from '../../redux/rootReducer';
import useDebounce, { searchFieldDebounceTimeMs } from '../../util/useDebounce';
import SetTable from '../browse/SetTable';

interface ConnectedSetTableProps {
  userId: string;
  expansionsFirst: number;
  expansionsSkip: number;
  expansionsPage: number;
  setExpansionsSkip: (skip: number) => void;
  setExpansionsFirst: (first: number) => void;
  setExpansionsPage: (page: number) => void;
}

export const ConnectedSetTable: React.FC<ConnectedSetTableProps> = ({
  userId,
  expansionsFirst,
  expansionsSkip,
  expansionsPage,
  setExpansionsSkip,
  setExpansionsFirst,
  setExpansionsPage,
}) => {
  const { sortExpansionBy, sortExpansionByDirection, priceType, expansionTypes, expansionCategories, expansionSearchQuery } = useSelector(
    (state: RootState) => state.collection
  );

  const debouncedExpansionSearchQuery = useDebounce(expansionSearchQuery, searchFieldDebounceTimeMs);

  const {
    data: collectionSummary,
    isLoading: isFilteredCollectionSummaryLoading,
    isFetching,
    error: filteredCollectionSummaryError,
  } = useGetFilteredCollectionSummaryLegacyQuery({
    userId,
    priceType,
    first: expansionsFirst,
    skip: expansionsSkip,
    search: debouncedExpansionSearchQuery,
    sortBy: sortExpansionBy,
    sortByDirection: sortExpansionByDirection,
    additionalSortBy: null,
    whereSetCompletionStatus: null,
    setTypes: expansionTypes,
    setCategories: expansionCategories,
  });

  const expansions = collectionSummary?.data?.filteredCollectionSummaryLegacy?.collectionSummary;
  const costsToPurchase = collectionSummary?.data?.filteredCollectionSummaryLegacy?.collectionSummary;
  const totalExpansionsResults = collectionSummary?.data?.filteredCollectionSummaryLegacy?.count ?? 0;

  return (
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
      userId={userId}
      isCollectorMode
      isFetching={isFetching}
    />
  );
};

const MemoizedSetTable = memo(SetTable);
