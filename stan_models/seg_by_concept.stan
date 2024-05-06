functions {
  matrix fill_matrix(vector rates, int J) {
    matrix[J,J] Q = rep_matrix(0,J,J);
    { int k = 1;
    for (i in 1:J) {
		  for (j in 1:J) {
		    if (i != j) {
		      Q[i,j] = rates[k];
			    k += 1;
		      }
		    }
		  }
	  }
    for (j in 1:J) {
      Q[j,j] = -sum(Q[j,]);
    }
    return(Q);
  }
  //compute likelihood via Felsenstein's Pruning Algorithm
  vector pruning( vector beta , vector theta , data real[] xr , data int[] xi ) {
    int N = xi[1];
    int J = xi[2];
    int B_ = xi[3];
    int B = N-1;
    int concept_ID = xi[4];
    int tiplik[N*J] = xi[5:(N*J+4)];
    int tiplik_absent[N*J] = xi[(5+N*J):(2*N*J+4)];
    int child[B] = xi[(2*N*J+5):(2*N*J+4+B)];
    int parent[B] = xi[(2*N*J+5+B):(2*N*J+4+2*B)];
    real brlen[B] = xr;
    vector[J*(J-1)] rates = theta[((concept_ID-1)*J*(J-1)+1):(concept_ID*J*(J-1))];
    matrix[J,J] Q = fill_matrix(rates,J);
    real log_lik;
    real log_lik_absent;
    real log_lik_corrected;
    matrix[N,J] lambda;
    matrix[N,J] lambda_absent;
    for (j in 1:J) {
      lambda[,j] = to_vector(log(tiplik[((N*j)-(N-1)):(N*j)]));
      lambda_absent[,j] = to_vector(log(tiplik_absent[((N*j)-(N-1)):(N*j)]));
    }
    {int k = 1;
    for (b in 1:B_) {
      matrix[J,J] P = matrix_exp(Q*brlen[b]); //via matrix exponentiation
      for (j in 1:J) {
        lambda[parent[b],j] += log_sum_exp(log(P[j])+lambda[child[b],]);
        lambda_absent[parent[b],j] += log_sum_exp(log(P[j])+lambda_absent[child[b],]);
        k += J - 1;
      }
    }
    }
    log_lik = log_sum_exp(-log(J) + lambda[parent[B_]]);
    log_lik_absent = log_sum_exp(-log(J) + lambda_absent[parent[B_]]);
    log_lik_corrected = log_lik - log(1-exp(log_lik_absent));
    return([log_lik_corrected]');
  }
}
data {
  int<lower=1> N;                           //number of nodes
  int<lower=1> B;                           //number of branches
  int<lower=1> D;                           //number of features
  int<lower=1> J;                           //number of states
  int<lower=1> L;
  int<lower=1> Bs[D];
  int<lower=1> K;
  int<lower=1,upper=K> concept[D];
  int<lower=1,upper=L> fam[D];
  int child[D,B];
  int parent[D,B];
  real brlen[D,B];
  int tiplik[D,N,J];
}
transformed data {
  //pack phylogenetic data into D 1-dim vectors
  int xi[D,2*N*J+4+2*B];
  real xr[D,B];
  vector[0] beta;
  for (d in 1:D) {
    xi[d,1] = N;
    xi[d,2] = J;
    xi[d,3] = Bs[d];
    xi[d,4] = concept[d];
    for (j in 1:J) {
      int pattern_abs[N];
      xi[d,((j-1)*N+5):(j*N+4)] = tiplik[d,,j];
      for (i in 1:N) {
        if (i <= Bs[d]+1) {
          pattern_abs[i] = 1;
        }
        else {
          pattern_abs[i] = 0;
        }
        if (j > 1) {
          if (i <= Bs[d]/2) {
            pattern_abs[i] = 0;
          }
        }
      }
      xi[d,((j-1)*N+J*N+5):(j*N+J*N+4)] = pattern_abs;
    }
    xi[d,(2*J*N+5):(2*J*N+B+4)] = child[d,];
    xi[d,(2*J*N+B+5):(2*J*N+2*B+4)] = parent[d,];
    xi[d] = to_array_1d(xi[d]);
    xr[d] = to_array_1d(brlen[d,]);
  }
}
parameters {
  vector[K*J*(J-1)] mu;
  vector<lower=0>[K*J*(J-1)] sigma;
  vector[K*J*(J-1)] eps_fam[L];
}
transformed parameters {
  vector[D] log_lik;
  { vector[K*J*(J-1)] theta[D];
    for (d in 1:D) {
      theta[d] = to_vector(exp(mu + sigma .* eps_fam[fam[d],]));
    }
    log_lik = map_rect(pruning,beta,theta,xr,xi);
  }
}
model {
  mu ~ normal(0,1);
  sigma ~ normal(0,1);
  for (l in 1:L) {
    eps_fam[l] ~ normal(0,1);
  }
  target += sum(log_lik);
}