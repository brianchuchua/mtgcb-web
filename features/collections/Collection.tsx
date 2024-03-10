import { memo, useEffect, useMemo } from 'react';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';
import Breadcrumbs from '../../components/layout/Breadcrumbs';
import { ResponsiveContainer } from '../../components/layout/ResponsiveContainer';
import { useGetAllSetsMetaQuery } from '../../network/services/mtgcbApi';
import { RootState } from '../../redux/rootReducer';
import { includesQuantityFilters } from '../../util/includesQuantityFilters';
import useDebounce, { searchFieldDebounceTimeMs } from '../../util/useDebounce';
import { usePagination } from '../../util/usePagination';
import CardGallery from '../browse/CardGallery';
import CardTable from '../browse/CardTable';
import { useCardSearch } from '../browse/hooks/useCardSearch';
import SetGallery from '../browse/SetGallery';
import SetTable from '../browse/SetTable';
import { setFormVisibility } from './collectionSlice';
import { ConnectedCollectionDetails } from './ConnectedCollectionDetails';
import { useCollectionByCardId } from './hooks/useCollectionByCardId';
import { useFilteredCardsSummary } from './hooks/useFilteredCardsSummary';
import { useFilteredCollectionSummary } from './hooks/useFilteredCollectionSummary';

export const Collection: React.FC<CollectionProps> = ({ userId }) => {
  const reduxSlice = 'collection';

  const {
    searchQuery,
    oracleTextQuery,
    artistQuery,
    cardStatSearches,
    sortBy,
    viewSubject,
    viewMode,
    expansionSearchQuery,
    sortExpansionBy,
    sortExpansionByDirection,
    expansionTypes,
    expansionCategories,
    includeSubsets,
    includeSubsetGroups,
    priceType,
    includeSubsetsInSets,
    cardSets,
  } = useSelector((state: RootState) => state[reduxSlice]);

  const {
    skip: expansionsSkip,
    setSkip: setExpansionsSkip,
    first: expansionsFirst,
    setFirst: setExpansionsFirst,
    page: expansionsPage,
    setPage: setExpansionsPage,
    handleTotalResultsChange: handleTotalResultsChangeExpansions,
  } = usePagination({
    initialPage: 1,
    initialPageSize: 20,
    localStorageKey: 'numberOfExpansionsPerPage',
  });

  const dispatch = useDispatch();

  useEffect(() => {
    dispatch(setFormVisibility({ isFormVisibile: true }));
    return function cleanUpForm() {
      dispatch(setFormVisibility({ isFormVisibile: false }));
    };
  }, []);

  const { data: allSetsMetaResponse } = useGetAllSetsMetaQuery({
    name: expansionSearchQuery,
    sortBy: sortExpansionBy,
    sortByDirection: sortExpansionByDirection,
    setTypes: expansionTypes,
    setCategories: expansionCategories,
    includeSubsets,
    includeSubsetGroups,
  });
  const allSetsMeta = allSetsMetaResponse?.data;
  const totalExpansionsResults = allSetsMeta?.count || 0;

  useEffect(() => {
    handleTotalResultsChangeExpansions(totalExpansionsResults);
  }, [expansionsSkip, totalExpansionsResults]);

  const debouncedSearchQuery = useDebounce(searchQuery, searchFieldDebounceTimeMs);
  const debouncedOracleTextQuery = useDebounce(oracleTextQuery, searchFieldDebounceTimeMs);
  const debouncedArtistQuery = useDebounce(artistQuery, searchFieldDebounceTimeMs);

  // Tech debt: If a user is trying to search or sort by quantity, we have to involve an external legacy database and do a manual remote join
  const hasQuantityFilters = useMemo(() => includesQuantityFilters(cardStatSearches, sortBy), [cardStatSearches, sortBy]);

  const {
    cards,
    isCardDataLoading,
    isCardDataFetching,
    isCardMetaDataLoading,
    isCardMetaDataFetching,
    skip,
    setSkip,
    page,
    setPage,
    first,
    setFirst,
    totalResults,
  } = useCardSearch(reduxSlice, null, hasQuantityFilters /* TODO: temporary */);
  const cardIds = cards?.map((card) => card.id);

  const { collectionByCardIdWithDefaults, isCollectionByCardIdLoading } = useCollectionByCardId(userId, cardIds, hasQuantityFilters);

  const {
    cardsFromHeavyQuery,
    collectionByCardIdWithDefaultsFromHeavyQuery,
    loadingFilteredCardsSummary,
    fetchingFilteredCardsSummary,
    totalResultsFromHeavyQuery,
  } = useFilteredCardsSummary(
    userId,
    reduxSlice,
    hasQuantityFilters,
    debouncedSearchQuery,
    debouncedOracleTextQuery,
    debouncedArtistQuery,
    first,
    skip,
    cardSets
  );

  const {
    expansions,
    costsToPurchase,
    isFilteredCollectionSummaryLoading,
    isFilteredCollectionSummaryFetching,
    username,
  } = useFilteredCollectionSummary(userId, reduxSlice, expansionsFirst, expansionsSkip);

  const isLoading = isCardDataLoading || isCardMetaDataLoading || isCollectionByCardIdLoading || loadingFilteredCardsSummary;
  const isFetching = isCardDataFetching || isCardMetaDataFetching || fetchingFilteredCardsSummary;

  return (
    <ResponsiveContainer maxWidth="xl">
      <Breadcrumbs
        links={[
          {
            title: username ? `${username}'s Collection` : '',
            url: userId ? `/collections/${userId}` : '',
          },
        ]}
      />
      <ConnectedCollectionDetails userId={userId} expansionsFirst={expansionsFirst} expansionsSkip={expansionsSkip} />
      <ContentWrapper>
        {viewSubject === 'cards' && viewMode === 'grid' && (
          <MemoizedCardGallery
            cards={hasQuantityFilters ? cardsFromHeavyQuery : cards}
            totalResults={hasQuantityFilters ? totalResultsFromHeavyQuery : totalResults}
            first={first}
            skip={skip}
            page={page}
            setSkip={setSkip}
            setFirst={setFirst}
            setPage={setPage}
            priceType={priceType}
            userId={userId}
            collectionByCardId={hasQuantityFilters ? collectionByCardIdWithDefaultsFromHeavyQuery : collectionByCardIdWithDefaults}
            isLoading={isLoading}
            isFetching={isFetching}
          />
        )}
        {viewSubject === 'cards' && viewMode === 'table' && (
          <MemoizedCardTable
            cards={hasQuantityFilters ? cardsFromHeavyQuery : cards}
            totalResults={hasQuantityFilters ? totalResultsFromHeavyQuery : totalResults}
            first={first}
            skip={skip}
            page={page}
            setSkip={setSkip}
            setFirst={setFirst}
            setPage={setPage}
            priceType={priceType}
            userId={userId}
            collectionByCardId={hasQuantityFilters ? collectionByCardIdWithDefaultsFromHeavyQuery : collectionByCardIdWithDefaults}
            isLoading={isLoading}
            isFetching={isFetching}
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
            isLoading={isFilteredCollectionSummaryLoading}
            isFetching={isFilteredCollectionSummaryFetching}
            includeSubsetsInSets={includeSubsetsInSets}
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
            userId={userId}
            isCollectorMode
            isFetching={isFetching}
          />
        )}
      </ContentWrapper>
    </ResponsiveContainer>
  );
};

const ContentWrapper = styled.div(({ theme }) => ({
  marginTop: theme.spacing(0),
}));

interface CollectionProps {
  userId: string;
}

const MemoizedCardGallery = memo(CardGallery);
const MemoizedCardTable = memo(CardTable);
const MemoizedSetGallery = memo(SetGallery);
const MemoizedSetTable = memo(SetTable);
