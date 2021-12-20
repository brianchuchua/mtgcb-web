import { useDispatch, useSelector } from 'react-redux';
import PriceTypeSelector from '../../../../components/search-form-components/PriceTypeSelector';
import { RootState } from '../../../../redux/rootReducer';
import { ConnectedSearchFormComponentProps } from './types';

interface ConnectedPriceTypeSelectorProps extends ConnectedSearchFormComponentProps {
  setPriceType: any;
}

const ConnectedPriceTypeSelector: React.FC<ConnectedPriceTypeSelectorProps> = ({ reduxSlice, setPriceType }) => {
  const dispatch = useDispatch();
  const { priceType } = useSelector((state: RootState) => state[reduxSlice]);

  const updatePriceType = (priceTypeValue) => {
    dispatch(setPriceType(priceTypeValue));
  };

  return <PriceTypeSelector priceType={priceType} setPriceType={updatePriceType} />;
};

export default ConnectedPriceTypeSelector;
