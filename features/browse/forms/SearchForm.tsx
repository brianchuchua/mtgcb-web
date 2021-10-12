import { useSelector } from 'react-redux';
import { RootState } from '../../../redux/rootReducer';
import {
  CardNameSearch,
  CardStatSearch,
  ColorSelector,
  OracleTextSearch,
  PriceTypeSelector,
  RaritySelector,
  SetSelector,
  ShowAllPrintingsToggle,
  SortSelector,
  TypeSelector,
  ViewModeSelector,
} from './search-form-components';

// TODO: Add headers to this thing
const SearchForm: React.FC = () => {
  const { isFormVisible } = useSelector((state: RootState) => state.browse);

  return (
    <>
      {isFormVisible && (
        <>
          <ViewModeSelector />
          <PriceTypeSelector />
          <CardNameSearch />
          <OracleTextSearch />
          <TypeSelector />
          <ColorSelector />
          <RaritySelector />
          <SetSelector />
          <CardStatSearch />
          <SortSelector />
          <ShowAllPrintingsToggle />
        </>
      )}
    </>
  );
};

export default SearchForm;
