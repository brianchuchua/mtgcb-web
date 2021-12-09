import { useSelector } from 'react-redux';
import { RootState } from '../../../redux/rootReducer';
import ConnectedCardNameSearch from './search-form-components/ConnectedCardNameSearch';
import ConnectedCardStatSearch from './search-form-components/ConnectedCardStatSearch';
import ConnectedColorSelector from './search-form-components/ConnectedColorSelector';
import ConnectedOracleTextSearch from './search-form-components/ConnectedOracleTextSearch';
import ConnectedPriceTypeSelector from './search-form-components/ConnectedPriceTypeSelector';
import ConnectedRaritySelector from './search-form-components/ConnectedRaritySelector';
import ConnectedSortSelector from './search-form-components/ConnectedSortSelector';
import ConnectedTypeSelector from './search-form-components/ConnectedTypeSelector';
import ConnectedViewModeSelector from './search-form-components/ConnectedViewModeSelector';

// TODO: Add headers to this thing
const SetSearchForm: React.FC = () => {
  const { isFormVisible, viewSubject } = useSelector((state: RootState) => state.set);

  return (
    <>
      {isFormVisible && viewSubject === 'cards' && (
        <>
          <ConnectedViewModeSelector showSubjectChangeSection={false} />
          <ConnectedPriceTypeSelector />
          <ConnectedCardNameSearch />
          <ConnectedOracleTextSearch />
          <ConnectedTypeSelector />
          <ConnectedColorSelector />
          <ConnectedRaritySelector />
          <ConnectedCardStatSearch />
          <ConnectedSortSelector />
        </>
      )}
    </>
  );
};

export default SetSearchForm;
