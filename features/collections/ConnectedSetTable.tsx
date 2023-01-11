import React, { memo } from 'react';
import { useSelector } from 'react-redux';
import { useGetFilteredCollectionSummaryLegacyQuery } from '../../network/services/mtgcbApi';
import { RootState } from '../../redux/rootReducer';
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
  const {
    sortExpansionBy,
    sortExpansionByDirection,
    priceType,
    expansionTypes,
    expansionCategories,
    expansionSearchQuery,
    setCompletionStatuses,
  } = useSelector((state: RootState) => state.collection);

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
    search: expansionSearchQuery,
    sortBy: sortExpansionBy,
    sortByDirection: sortExpansionByDirection,
    additionalSortBy: null,
    whereSetCompletionStatus: setCompletionStatuses ?? ['all'],
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
