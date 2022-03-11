import { useSelector } from 'react-redux';
import { RootState } from '../../../redux/rootReducer';
import { CardEditingModeSelector, PriceTypeSelector } from '../../search/forms/connected-form-components';
import { setCardEditingMode, setPriceType } from '../editCardsSlice';

const SetCollectionSearchForm: React.FC = () => {
  const { isFormVisible } = useSelector((state: RootState) => state.editCards);
  const reduxSlice = 'editCards';

  return (
    <>
      {isFormVisible && (
        <>
          <CardEditingModeSelector reduxSlice={reduxSlice} setCardEditingMode={setCardEditingMode} />
          <PriceTypeSelector reduxSlice={reduxSlice} setPriceType={setPriceType} />
        </>
      )}
    </>
  );
};

export default SetCollectionSearchForm;
