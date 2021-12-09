import { useDispatch, useSelector } from 'react-redux';
import PriceTypeSelector from '../../../../components/search-form-components/PriceTypeSelector';
import { RootState } from '../../../../redux/rootReducer';
import { setPriceType } from '../../setSlice';

const ConnectedPriceTypeSelector: React.FC = () => {
  const dispatch = useDispatch();
  const { priceType } = useSelector((state: RootState) => state.set);

  const updatePriceType = (priceTypeValue) => {
    dispatch(setPriceType(priceTypeValue));
  };

  return <PriceTypeSelector priceType={priceType} setPriceType={updatePriceType} />;
};

export default ConnectedPriceTypeSelector;
