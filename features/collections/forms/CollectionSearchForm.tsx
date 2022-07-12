import { useSelector } from 'react-redux';
import { RootState } from '../../../redux/rootReducer';
import {
  CardNameSearch,
  CardStatSearch,
  ColorSelector,
  OracleTextSearch,
  PriceTypeSelector,
  RaritySelector,
  SetCategorySelector,
  SetCompletionStatusSelector,
  SetNameSearch,
  SetSelector,
  SetSortSelector,
  SetTypeSelector,
  SortSelector,
  TypeSelector,
  ViewModeSelector,
} from '../../search/forms/connected-form-components';
import {
  addCardStatSearch,
  removeCardStatSearch,
  searchAttributeOptions,
  setCardColors,
  setCardRarities,
  setCardSets,
  setCardSort,
  setCardSortDirection,
  setCardStatSearches,
  setCardTypes,
  setColorType,
  setComparator,
  setExpansionCategories,
  setExpansionSearchQuery,
  setExpansionSort,
  setExpansionSortDirection,
  setExpansionTypes,
  setOracleTextQuery,
  setPriceType,
  setSearchAttribute,
  setSearchQuery,
  setSetCompletionStatuses,
  setViewMode,
  setViewSubject,
  sortByOptions,
} from '../collectionSlice';

// TODO: Add headers to this thing
const CollectionSearchForm: React.FC = () => {
  const { isFormVisible, viewSubject } = useSelector((state: RootState) => state.collection);
  const reduxSlice = 'collection';

  return (
    <>
      {isFormVisible && viewSubject === 'cards' && (
        <>
          <ViewModeSelector reduxSlice={reduxSlice} setViewMode={setViewMode} setViewSubject={setViewSubject} />
          <PriceTypeSelector reduxSlice={reduxSlice} setPriceType={setPriceType} />
          <CardNameSearch reduxSlice={reduxSlice} setSearchQuery={setSearchQuery} />
          <OracleTextSearch reduxSlice={reduxSlice} setOracleTextQuery={setOracleTextQuery} />
          <TypeSelector reduxSlice={reduxSlice} setCardTypes={setCardTypes} />
          <ColorSelector reduxSlice={reduxSlice} setColorType={setColorType} setCardColors={setCardColors} />
          <RaritySelector reduxSlice={reduxSlice} setCardRarities={setCardRarities} />
          <SetSelector reduxSlice={reduxSlice} setCardSets={setCardSets} />
          <CardStatSearch
            reduxSlice={reduxSlice}
            addCardStatSearch={addCardStatSearch}
            removeCardStatSearch={removeCardStatSearch}
            setCardStatSearches={setCardStatSearches}
            setComparator={setComparator}
            setSearchAttribute={setSearchAttribute}
            searchAttributeOptionsOverride={searchAttributeOptions}
          />
          <SortSelector
            reduxSlice={reduxSlice}
            sortByOptions={sortByOptions}
            setCardSort={setCardSort}
            setCardSortDirection={setCardSortDirection}
          />
        </>
      )}
      {isFormVisible && viewSubject === 'sets' && (
        <>
          <ViewModeSelector reduxSlice={reduxSlice} setViewMode={setViewMode} setViewSubject={setViewSubject} />
          <PriceTypeSelector reduxSlice={reduxSlice} setPriceType={setPriceType} />
          <SetCompletionStatusSelector reduxSlice={reduxSlice} setCompletionStatuses={setSetCompletionStatuses} />
          <SetNameSearch reduxSlice={reduxSlice} setExpansionSearchQuery={setExpansionSearchQuery} />
          <SetCategorySelector reduxSlice={reduxSlice} setExpansionCategories={setExpansionCategories} />
          <SetTypeSelector reduxSlice={reduxSlice} setExpansionTypes={setExpansionTypes} />
          <SetSortSelector
            reduxSlice={reduxSlice}
            setExpansionSort={setExpansionSort}
            setExpansionSortDirection={setExpansionSortDirection}
          />
        </>
      )}
    </>
  );
};

export default CollectionSearchForm;
