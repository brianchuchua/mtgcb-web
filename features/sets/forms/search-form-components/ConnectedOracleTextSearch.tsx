import { useDispatch, useSelector } from 'react-redux';
import OracleTextSearch from '../../../../components/search-form-components/OracleTextSearch';
import { RootState } from '../../../../redux/rootReducer';
import { setOracleTextQuery } from '../../setSlice';

const ConnectedOracleTextSearch: React.FC = () => {
  const dispatch = useDispatch();
  const { oracleTextQuery } = useSelector((state: RootState) => state.set);

  const updateOracleTextQuery = (newOracleTextQuery: string) => {
    dispatch(setOracleTextQuery({ oracleTextQuery: newOracleTextQuery }));
  };

  return <OracleTextSearch oracleTextQuery={oracleTextQuery} updateOracleTextQuery={updateOracleTextQuery} />;
};

export default ConnectedOracleTextSearch;
